{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies, FlexibleContexts, RecordWildCards, ViewPatterns #-}
module Graphics where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.List
import qualified Data.Vector as V
import qualified Data.Trie as T
import Data.Maybe
import "lens" Control.Lens
import Control.Monad.IO.Class

import Material hiding (Blending)

import Graphics.GPipe as GPipe

mkColor wt uni ca sa (V4 rV gV bV aV) = V4 cr cg cb alpha
  where
    green = V3 0 1 0
    V3 cr cg cb = case saRGBGen sa of
        RGB_Wave w              -> let c = mkWave wt uni w in V3 c c c
        RGB_Const r g b         -> V3 (realToFrac r) (realToFrac g) (realToFrac b)
        RGB_Identity            -> V3 1 1 1
        RGB_IdentityLighting    -> V3 (identityLight_ uni) (identityLight_ uni) (identityLight_ uni)
        RGB_Entity              -> entityRGB uni
        RGB_OneMinusEntity      -> V3 1 1 1 - entityRGB uni
        RGB_ExactVertex         -> V3 rV gV bV
        RGB_Vertex              -> V3 rV gV bV ^* identityLight_ uni
        RGB_LightingDiffuse     -> green -- TODO
        {-  input:
                entity: ambientLight
                        directedLight
                        lightDir
                model:  position
                        normal
        -}
        RGB_OneMinusVertex      -> V3 1 1 1 - V3 rV gV bV ^* identityLight_ uni

    alpha = case saAlphaGen sa of
        A_Wave w            -> let a = mkWave wt uni w in clamp a 0 1
        A_Const a           -> realToFrac a
        A_Portal            -> 1 -- TODO
        A_Identity          -> 1
        A_Entity            -> entityAlpha uni
        A_OneMinusEntity    -> 1 - entityAlpha uni
        A_Vertex            -> aV
        A_LightingSpecular  -> 1 -- TODO
        {-  input:
                model:  position
                        normal
                user:   viewOrigin
        -}
        A_OneMinusVertex    -> 1 - aV

mkWave' wt uni off (Wave wFunc base amplitude phase freq) = realToFrac base + a * realToFrac amplitude
  where
    u   = off + realToFrac phase + realToFrac freq * time uni :: VFloat
    v   = sample1D (smp wt) (SampleLod 0) Nothing Nothing u
    a   = v * 2 - 1
    smp = case wFunc of
        WT_Sin              -> wtSin
        WT_Triangle         -> wtTriangle
        WT_Square           -> wtSquare
        WT_Sawtooth         -> wtSawTooth
        WT_InverseSawtooth  -> wtInverseSawTooth
        WT_Noise            -> wtNoise

mkWave wt uni w = mkWave' wt uni 0 w :: VFloat

mkDeform wt uni uv normal pos d = case d of
    D_Move v w -> pos + (realToFrac <$> v) ^* mkWave wt uni w
    D_Wave spread w@(Wave _ _ _ _ f)
        | f < 0.000001  -> pos + normal ^* mkWave wt uni w
        | otherwise     ->
            let V3 x y z    = pos
                off         = (x + y + z) * realToFrac spread
            in pos + normal ^* mkWave' wt uni off w
    D_Bulge w h s   -> let V2 u _   = uv
                           now      = time uni * realToFrac s
                           off      = u * realToFrac w + now
                       in pos + normal ^* (sin off * realToFrac h)
    _ -> pos

mkTCMod wt uni pos uv m = case m of
    TM_Scroll su sv -> uv + V2 (realToFrac su) (realToFrac sv) ^* time uni
    TM_Scale su sv  -> uv * V2 (realToFrac su) (realToFrac sv)
    TM_Stretch w    -> let p    = 1 / mkWave wt uni w
                           off  = 0.5 - 0.5 * p
                       in uv ^* p + V2 off off
    TM_Rotate speed -> let fi   = (-realToFrac speed * pi / 180) * time uni
                           s    = sin fi
                           ms   = s * (-1)
                           c    = cos fi
                           mA   = V2 c s
                           mB   = V2 ms c
                           m    = V2 mA mB
                           off  = V2 (0.5 - 0.5 * c + 0.5 * s) (0.5 - 0.5 * s - 0.5 * c)
                       in uv *! m + off
    TM_Transform m00 m01 m10 m11 t0 t1  -> let V2 u v   = uv
                                               u'       = u * realToFrac m00 + v * realToFrac m10 + realToFrac t0
                                               v'       = u * realToFrac m01 + v * realToFrac m11 + realToFrac t1
                                           in V2 u' v'
    TM_Turb base amp phase freq ->  let V2 u v      = uv
                                        V3 x y z    = pos
                                        now         = realToFrac phase + time uni * realToFrac freq
                                        offU        = (2 * pi) * ((x + z) * (0.125 / 128) + now)
                                        offV        = (2 * pi) * (y * (0.125 / 128) + now)
                                    in uv + sin (V2 offU offV) ^* realToFrac amp
    _ -> uv

mkTexCoord wt uni pos normal sa uvD uvL = foldl' (mkTCMod wt uni pos) uv $ saTCMod sa
  where
    uv = case saTCGen sa of
        TG_Base         -> uvD
        TG_Lightmap     -> uvL
        TG_Environment  ->  let viewer      = signorm $ viewOrigin uni - pos
                                d           = normal `dot` viewer
                                V3 _ y z    = normal ^* (2 * d) - viewer
                            in V2 (0.5 + y * 0.5) (0.5 - z * 0.5)
        TG_Vector s t   -> V2 (pos `dot` (realToFrac <$> s)) (pos `dot` (realToFrac <$> t))

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

windowSize      (a,(_,_,_,_,_,_,_)) = a
entityRGB       (_,(a,_,_,_,_,_,_)) = a
entityAlpha     (_,(_,a,_,_,_,_,_)) = a
identityLight_  (_,(_,_,a,_,_,_,_)) = a
time            (_,(_,_,_,a,_,_,_)) = a
viewOrigin      (_,(_,_,_,_,a,_,_)) = a
viewTarget      (_,(_,_,_,_,_,a,_)) = a
viewUp          (_,(_,_,_,_,_,_,a)) = a
{-
uniform tuple:
      entityRGB, entityAlpha, identityLight, time,  viewOrigin, viewTarget, viewUp
      (V3 Float, Float,       Float,         Float, V3 Float,   V3 Float,   V3 Float)
-}

mkVertexShader wt uni ca sa (li, (p,n,d,l,c)) = (screenPos, (uv, color, Flat li))
  where
    viewMat     = lookAt' (viewOrigin uni) (viewTarget uni) (viewUp uni)
    V2 w h      = windowSize uni
    projMat     = perspective (pi/3) (toFloat w / toFloat h) 1 10000
    screenPos   = projMat !*! viewMat !* V4 x y z 1
    pos@(V3 x y z) = foldl' (mkDeform wt uni d n) p $ caDeformVertexes ca
    uv    = mkTexCoord wt uni pos n sa d l
    color = mkColor wt uni ca sa c

mkFragmentShader dSmp lSmp ca sa (uv,rgba, li) = case saTexture sa of
    ST_WhiteImage   -> rgba
    ST_Lightmap     -> rgba * texColor3 lSmp
    ST_Map {}       -> rgba * texColor4 dSmp
    ST_ClampMap {}  -> rgba * texColor4 dSmp
    ST_AnimMap {}   -> rgba * texColor4 dSmp
  where
    lod = if caNoMipMaps ca then SampleLod 0 else SampleAuto
    texColor4 smp = sample2D smp lod Nothing Nothing uv
    texColor3 smp = v3v4 $ sample2DArray smp lod Nothing (v2v3 uv li)
    v2v3 (V2 u v) i = V3 u v i
    v3v4 (V3 r g b) = V4 r g b 1

mkFilterFunction dSmp lSmp ca sa uvrgba = case saAlphaFunc sa of
    Nothing -> true
    Just f  -> let V4 _ _ _ a = mkFragmentShader dSmp lSmp ca sa uvrgba
               in case f of
                 A_Gt0   -> a >* 0
                 A_Lt128 -> a GPipe.<* 0.5
                 A_Ge128 -> a >=* 0.5

mkRasterContext ca (riScreenSize -> V2 w h) = (cull, ViewPort (V2 0 0) (V2 w h), DepthRange 0 1)
  where
    -- TODO: offset  = if caPolygonOffset ca then Offset (-1) (-2) else NoOffset
    cull    = case caCull ca of
        CT_FrontSided   -> Back --Front
        CT_BackSided    -> Front --Back
        CT_TwoSided     -> FrontAndBack

mkAccumulationContext :: StageAttrs -> (ContextColorOption RGBAFloat, DepthOption)
mkAccumulationContext StageAttrs{..} = (ContextColorOption blend (pure True), DepthOption depthFunc saDepthWrite)
  where
    depthFunc   = case saDepthFunc of
        D_Equal     -> Equal
        D_Lequal    -> Lequal
    blend       = case saBlend of
        Nothing     -> NoBlending
        Just (src,dst)  -> BlendRgbAlpha (FuncAdd,FuncAdd) (BlendingFactors srcF dstF, BlendingFactors srcF dstF) (V4 1 1 1 1)
          where
            srcF    = cvt src
            dstF    = cvt dst
    cvt b = case b of
        B_DstAlpha          -> DstAlpha
        B_DstColor          -> DstColor
        B_One               -> One
        B_OneMinusDstAlpha  -> OneMinusDstAlpha
        B_OneMinusDstColor  -> OneMinusDstColor
        B_OneMinusSrcAlpha  -> OneMinusSrcAlpha
        B_OneMinusSrcColor  -> OneMinusSrcColor
        B_SrcAlpha          -> SrcAlpha
        B_SrcAlphaSaturate  -> SrcAlphaSaturate
        B_SrcColor          -> SrcColor
        B_Zero              -> Zero

mkStage lightmapArray checkerTex texInfo wt uni ca sa = do
  let (edge,diffuse) = case saTexture sa of
        ST_WhiteImage   -> (Repeat,       checkerTex)
        ST_Lightmap     -> (ClampToEdge,  checkerTex)
        ST_Map n        -> (Repeat,       lookupTex n)
        ST_ClampMap n   -> (ClampToEdge,  lookupTex n)
        ST_AnimMap _ (n:l)  -> (Repeat,   lookupTex n) -- TODO
      lookupTex n = maybe checkerTex id $ T.lookup n texInfo
  diffuseSmp  <- newSampler2D (\s -> (diffuse,      SamplerFilter Linear Linear Linear Nothing, (pure edge, undefined)))
  lightmapSmp <- newSampler2DArray (\s -> (lightmapArray, SamplerFilter Linear Linear Linear Nothing, (pure edge, undefined)))

  primitiveStream <- toPrimitiveStream riStream
  fragmentStream <- rasterize (mkRasterContext ca) (mkVertexShader wt uni ca sa <$> primitiveStream)
  let filteredFragmentStream = filterFragments (mkFilterFunction diffuseSmp lightmapSmp ca sa) fragmentStream
  --drawContextColorDepth (const $ mkAccumulationContext sa) $ withRasterizedInfo (\a r -> (a, rasterizedFragCoord r ^. _z + if caPolygonOffset ca then -2 else 0)) (mkFragmentShader sa <$> filteredFragmentStream)
  drawContextColorDepth (const $ mkAccumulationContext sa) $
    withRasterizedInfo
      (\a r -> (a, rasterizedFragCoord r ^. _z))
      (mkFragmentShader diffuseSmp lightmapSmp ca sa <$> filteredFragmentStream)

data WaveTable
  = WaveTable
  { wtSin             :: Sampler1D (Format RFloat)
  , wtSquare          :: Sampler1D (Format RFloat)
  , wtSawTooth        :: Sampler1D (Format RFloat)
  , wtInverseSawTooth :: Sampler1D (Format RFloat)
  , wtTriangle        :: Sampler1D (Format RFloat)
  , wtNoise           :: Sampler1D (Format RFloat)
  }

{-
type AttInput = (B3 Float, B3 Float, B2 Float, B2 Float, B4 Float)
type A = PrimitiveArray Triangles AttInput
type Tables os = (Texture1D os (Format RFloat), Texture1D os (Format RFloat), Texture1D os (Format RFloat), Texture1D os (Format RFloat), Texture1D os (Format RFloat))
-}
data RenderInput os
  = RenderInput
  { riScreenSize  :: V2 Int
  , riStream      :: PrimitiveArray Triangles (Float, (B3 Float, B3 Float, B2 Float, B2 Float, B4 Float)) --(Float, AttInput)
  }

mkShader lightMap checkerTex texInfo wt uni ca = mapM_ (mkStage lightMap checkerTex texInfo wt uni ca) $ caStages ca
