{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies, FlexibleContexts, RecordWildCards #-}
module Render where

import Control.Monad.Exception (MonadException)
import Control.Monad.IO.Class
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import qualified "GLFW-b" Graphics.UI.GLFW as GLFWb
import "lens" Control.Lens
import Control.Monad (unless,zipWithM)
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

type CF = ContextFormat RGBFloat Depth
type A = PrimitiveArray Triangles (B3 Float, B4 Float)

missingMaterial :: (MonadIO m, MonadException m) => Buffer os (Uniform (B3 Float, B3 Float, B3 Float)) -> ContextT w os CF m (CompiledShader os CF A)
missingMaterial uniformBuffer = do 
 liftIO (putStr "-")
 compileShader $ do
  (eye,center,up) <- getUniform (const (uniformBuffer,0))
  fragmentStream <- getProjectedFragments 600 eye center up id
  let fragmentStream2 = fmap ((\(_,V4 r g b a) -> V3 r g b)) fragmentStream
      fragmentStream3 = withRasterizedInfo (\a r -> (a, rasterizedFragCoord r ^. _z)) fragmentStream2
  --drawContextColor (const (ContextColorOption NoBlending (pure True))) fragmentStream2
  drawContextColorDepth (const (ContextColorOption NoBlending (pure True),DepthOption Lequal True)) fragmentStream3

--------------
-- Copy of lookAt from linear with normalize replaced with signorm 
lookAt' eye center up =
  V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
     (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
     (V4 (-za^._x) (-za^._y) (-za^._z) zd)
     (V4 0         0         0          1)
  where za = signorm $ center - eye
        xa = signorm $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye

make3d eye center up (V3 x y z, _) = projMat !*! viewMat !* V4 x y z 1
  where
    viewMat = lookAt' eye center up
    projMat = perspective (pi/3) 1 1 10000

getProjectedFragments size eye center up sf = do
  primitiveStream <- toPrimitiveStream sf
  let primitiveStream2 = fmap (\pos2d -> (make3d eye center up pos2d, pos2d)) primitiveStream
  rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 size size), DepthRange 0 1)) primitiveStream2

compileMaterial :: (MonadIO m, MonadException m) => Buffer os (Uniform (B3 Float, B3 Float, B3 Float)) -> CommonAttrs -> ContextT w os CF m (CompiledShader os CF A)
compileMaterial uniformBuffer shaderInfo = do
 liftIO (putStr ".")
 compileShader $ do
  (eye,center,up) <- getUniform (const (uniformBuffer,0))
  fragmentStream <- getProjectedFragments 600 eye center up id
  let fragmentStream2 = fmap ((\(_,V4 r g b a) -> V3 r g b)) fragmentStream
      fragmentStream3 = withRasterizedInfo (\a r -> (a, rasterizedFragCoord r ^. _z)) fragmentStream2
  drawContextColorDepth (const (ContextColorOption NoBlending (pure True),DepthOption Lequal True)) fragmentStream3
--------------

renderQuake :: Vec3 -> BSPLevel -> T.Trie CommonAttrs -> IO ()
renderQuake startPos bsp@BSPLevel{..} shaderInfo =
  runContextT GLFW.newContext (ContextFormatColorDepth RGB32F Depth32F) $ do
    -- pre tesselate patches and append to static draw verices and indices
    let patches     = map (tessellatePatch blDrawVertices 5) $ V.toList blSurfaces
        (verticesQ3,indicesQ3) = mconcat $ (blDrawVertices,blDrawIndices):patches
        patchSize   = [(V.length v, V.length i) | (v,i) <- patches]
        patchOffset = scanl' (\(offsetV,offsetI) (v,i) -> (offsetV + v, offsetI + i)) (V.length blDrawVertices, V.length blDrawIndices) patchSize
        patchInfo   = zip patchOffset patchSize

    vertexBufferQ3 :: Buffer os (B3 Float, B4 Float) <- newBuffer (V.length verticesQ3)
    writeBuffer vertexBufferQ3 0 [ (V3 x y z, V4 r g b a)
                                 | DrawVertex{..} <- V.toList verticesQ3
                                 , let Vec3 x y z = dvPosition
                                 , let Vec4 r g b a = dvColor
                                 ]

    indexBufferQ3 :: Buffer os (B Word32) <- newBuffer $ V.length indicesQ3
    writeBuffer indexBufferQ3 0 $ map fromIntegral $ V.toList indicesQ3 

    uniformBuffer :: Buffer os (Uniform (B3 Float, B3 Float, B3 Float)) <- newBuffer 1

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
  writeBuffer uniformBuffer 0 [(toV3 eye, toV3 center, toV3 up)]

  --liftIO $ putStrLn "Hello 3"
  mapM_ render renderings
  swapContextBuffers
  closeRequested <- GLFW.windowShouldClose
  unless closeRequested $ do
    liftIO $ do
      GLFWb.pollEvents
    renderLoop uniformBuffer s' renderings

