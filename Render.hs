{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies, FlexibleContexts, RecordWildCards #-}
module Render where

import Control.Monad.Exception (MonadException)
import Control.Monad.IO.Class
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import qualified "GLFW-b" Graphics.UI.GLFW as GLFWb
import "lens" Control.Lens
import Control.Monad (unless,zipWithM,forM)
import Data.Word (Word32)
import Control.Applicative (pure)
import Data.Monoid (mappend)

import qualified Data.Map as Map
import qualified Data.Trie as T
import qualified Data.Vector as V
import Data.Vect hiding (Vector)
import Data.Bits
import BSP
import Camera
import Q3Patch
import Material
import Graphics

import Data.List (foldl',scanl')

tessellatePatch :: V.Vector DrawVertex -> Int -> Surface -> (V.Vector DrawVertex,V.Vector Int)
tessellatePatch drawV level sf@Surface{..} = case srSurfaceType of
  Patch -> (V.concat vl,V.concat il)
    where
      (w,h)   = srPatchSize
      gridF :: [DrawVertex] -> [[DrawVertex]]
      gridF l = case splitAt w l of
          (x,[])  -> [x]
          (x,xs)  -> x:gridF xs
      grid        = gridF $ V.toList $ V.take srNumVertices $ V.drop srFirstVertex drawV
      controls    = [V.fromList $ concat [take 3 $ drop x l | l <- lines] | x <- [0,2..w-3], y <- [0,2..h-3], let lines = take 3 $ drop y grid]
      patches     = [tessellate c level | c <- controls]
      (vl,il)     = unzip $ reverse $ snd $ foldl' (\(o,l) (v,i) -> (o+V.length v, (v,V.map (+o) i):l)) (0,[]) patches
  _ -> mempty

convertSurface BSPLevel{..} indexBufferQ3 vertexBufferQ3 patchInfo sf@Surface{..} = do
  let Shader name sfFlags _ = blShaders V.! srShaderNum
      noDraw = sfFlags .&. 0x80 /= 0
      emitStream prim firstVertex numVertices firstIndex numIndices = if noDraw then return (0,mempty) else do
        vertexArrayQ3 <- (takeVertices numVertices . dropVertices firstVertex) <$> newVertexArray vertexBufferQ3
        indexArrayQ3 <- (takeIndices numIndices . dropIndices firstIndex) <$> newIndexArray indexBufferQ3 Nothing
        return (srShaderNum,toPrimitiveArrayIndexed prim indexArrayQ3 vertexArrayQ3)
  case srSurfaceType of
    Flare -> return (srShaderNum,mempty)
    Patch -> let ((firstVertex,firstIndex),(numVertices,numIndices)) = patchInfo
             in emitStream TriangleStrip firstVertex numVertices firstIndex numIndices
    _ -> emitStream TriangleList srFirstVertex srNumVertices srFirstIndex srNumIndices

type CF = ContextFormat RGBAFloat Depth
type AttInput = (B3 Float, B3 Float, B2 Float, B2 Float, B4 Float)
type A = PrimitiveArray Triangles AttInput
type UniInput = (B3 Float, B Float, B Float, B Float, B3 Float, B3 Float, B3 Float)

missingMaterial :: (MonadIO m, MonadException m) => Buffer os (Uniform UniInput) -> ContextT w os CF m (CompiledShader os CF A)
missingMaterial uniformBuffer = do 
 liftIO (putStr "-")
 compileShader $ do
  uni <- getUniform (const (uniformBuffer,0))
  primitiveStream <- toPrimitiveStream id
  let viewMat = lookAt' (viewOrigin uni) (viewTarget uni) (viewUp uni)
      projMat = perspective (pi/3) 1 1 10000
      make3d (V3 x y z) = projMat !*! viewMat !* V4 x y z 1
      primitiveStream2 = fmap (\(pos,_,_,_,color) -> (make3d pos, color)) primitiveStream
      size = 600
  fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 size size), DepthRange 0 1)) primitiveStream2
  let fragmentStream2 = withRasterizedInfo (\a r -> (a, rasterizedFragCoord r ^. _z)) fragmentStream
  drawContextColorDepth (const (ContextColorOption NoBlending (pure True),DepthOption Lequal True)) fragmentStream2

--mkShader uni ca = mapM_ (mkStage uni ca) $ caStages ca
compileMaterial :: (MonadIO m, MonadException m) => Buffer os (Uniform UniInput) -> CommonAttrs -> ContextT w os CF m (CompiledShader os CF A)
compileMaterial uniformBuffer shaderInfo = do
 liftIO (putStr ".")
{-
 stages <- forM (caStages shaderInfo) $ \sa -> compileShader $ do
  uni <- getUniform (const (uniformBuffer,0))
  mkStage uni shaderInfo sa
 return $ \s -> mapM_ ($ s) stages
-}
 compileShader $ do
  uni <- getUniform (const (uniformBuffer,0))
  mkShader uni shaderInfo

renderQuake :: Vec3 -> BSPLevel -> T.Trie CommonAttrs -> IO ()
renderQuake startPos bsp@BSPLevel{..} shaderInfo =
  runContextT GLFW.newContext (ContextFormatColorDepth RGBA32F Depth32F) $ do
    -- pre tesselate patches and append to static draw verices and indices
    let patches     = map (tessellatePatch blDrawVertices 5) $ V.toList blSurfaces
        (verticesQ3,indicesQ3) = mconcat $ (blDrawVertices,blDrawIndices):patches
        patchSize   = [(V.length v, V.length i) | (v,i) <- patches]
        patchOffset = scanl' (\(offsetV,offsetI) (v,i) -> (offsetV + v, offsetI + i)) (V.length blDrawVertices, V.length blDrawIndices) patchSize
        patchInfo   = zip patchOffset patchSize
        v2 (Vec2 x y) = V2 x y
        v3 (Vec3 x y z) = V3 x y z
        v4 (Vec4 x y z w) = V4 x y z w

    vertexBufferQ3 :: Buffer os AttInput <- newBuffer (V.length verticesQ3)
    writeBuffer vertexBufferQ3 0 [(v3 dvPosition, v3 dvNormal, v2 dvDiffuseUV, v2 dvLightmaptUV, v4 dvColor) | DrawVertex{..} <- V.toList verticesQ3]

    indexBufferQ3 :: Buffer os (B Word32) <- newBuffer $ V.length indicesQ3
    writeBuffer indexBufferQ3 0 $ map fromIntegral $ V.toList indicesQ3 

    uniformBuffer :: Buffer os (Uniform UniInput) <- newBuffer 1

    -- shader for unknown materials
    missingMaterialShader <- missingMaterial uniformBuffer

    --  create shader for each material
    usedShaders <- mapM (\Shader{..} -> maybe (return missingMaterialShader) (compileMaterial uniformBuffer) $ T.lookup shName shaderInfo) blShaders
    let surfaces = do
          surface0 <- zipWithM (convertSurface bsp indexBufferQ3 vertexBufferQ3) patchInfo $ V.toList blSurfaces
          -- group surfaces by material
          let surface1 = Map.unionsWith mappend . map (uncurry Map.singleton) $ surface0
              surface2 = Map.mapWithKey (\shader surface -> (caSort <$> T.lookup (shName $ blShaders V.! shader) shaderInfo, (usedShaders V.! shader) surface)) surface1
              -- sort surfaces by render queue
              sorted = concat $ Map.elems $ Map.unionsWith mappend [Map.singleton k [v] | (k,v) <- Map.elems surface2]
              allSurf = sequence_ sorted
          allSurf

    liftIO $ putStrLn "Hello 1"
    renderLoop uniformBuffer (s0 startPos) [
      do
        --liftIO $ putStrLn "Hello 2"
        clearContextColor 0
        clearContextDepth 10000
        surfaces
      ]

renderLoop uniformBuffer s renderings = do
  -- read input
  (mx,my) <- GLFW.getCursorPos
  let keyIsPressed k = fmap (== GLFW.KeyState'Pressed) $ GLFW.getKey k
  keys <- (,,,,) <$> keyIsPressed GLFW.Key'Left
                 <*> keyIsPressed GLFW.Key'Up
                 <*> keyIsPressed GLFW.Key'Down
                 <*> keyIsPressed GLFW.Key'Right
                 <*> keyIsPressed GLFW.Key'RightShift
  dt <- liftIO $ do
    Just t <- GLFWb.getTime
    GLFWb.setTime 0
    return t

  let s'@(eye,center,up,_) = calcCam dt (realToFrac mx, realToFrac my) keys s
      toV3 (Vec3 x y z) = V3 x y z
      viewMat = lookAt' (toV3 eye) (toV3 center) (toV3 up)
      projMat = perspective (pi/3::Float) 1 1 10000
      mvp = projMat !*! viewMat
{-
  uniform tuple:
      entityRGB, entityAlpha, identityLight, time,  viewOrigin, viewTarget, viewUp
      (V3 Float, Float,       Float,         Float, V3 Float,   V3 Float,   V3 Float)
-}
  writeBuffer uniformBuffer 0 [(V3 1 1 1, 1, 1, 0, toV3 eye, toV3 center, toV3 up)]

  --liftIO $ putStrLn "Hello 3"
  mapM_ render renderings
  swapContextBuffers
  closeRequested <- GLFW.windowShouldClose
  unless closeRequested $ do
    liftIO $ do
      GLFWb.pollEvents
    renderLoop uniformBuffer s' renderings

