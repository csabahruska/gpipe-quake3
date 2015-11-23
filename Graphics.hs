{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies, FlexibleContexts, RecordWildCards #-}
module Graphics where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import Data.List
--import Data.Digest.CRC32
import Data.Maybe
import Data.Vect
import "lens" Control.Lens

import Material hiding (Blending)

import Graphics.GPipe as GPipe

mkColor uni ca sa (V4 rV gV bV aV) = V4 cr cg cb alpha
  where
    green = V3 0 1 0
    V3 cr cg cb = case saRGBGen sa of
        RGB_Wave w              -> let c = mkWave uni w in V3 c c c
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
        A_Wave w            -> let a = mkWave uni w in clamp a 0 1
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

mkWave' uni off (Wave wFunc base amplitude phase freq) = realToFrac base + a * realToFrac amplitude :: VFloat
  where
    u           = off + realToFrac phase + realToFrac freq * time uni
    uv          = V2 u 0
    V4 v _ _ _  = V4 1 1 1 1 --TODO unpack' $ texture' sampler uv
    a           = v * 2 - 1
    --sampler     = Sampler LinearFilter Repeat $ TextureSlot name (Texture2D (Float RGBA) n1)
    name        = case wFunc of
        WT_Sin              -> "SinTable"
        WT_Triangle         -> "TriangleTable"
        WT_Square           -> "SquareTable"
        WT_Sawtooth         -> "SawToothTable"
        WT_InverseSawtooth  -> "InverseSawToothTable"
        WT_Noise            -> "Noise"

mkWave uni w = mkWave' uni 0 w :: VFloat

mkDeform uni uv normal pos d = case d of
    D_Move (Vec3 x y z) w   -> pos + V3 (realToFrac x) (realToFrac y) (realToFrac z) ^* mkWave uni w
    D_Wave spread w@(Wave _ _ _ _ f)
        | f < 0.000001  -> pos + normal ^* mkWave uni w
        | otherwise     ->
            let V3 x y z    = pos
                off         = (x + y + z) * realToFrac spread
            in pos + normal ^* mkWave' uni off w
    D_Bulge w h s   -> let V2 u _   = uv
                           now      = time uni * realToFrac s
                           off      = u * realToFrac w + now
                       in pos + normal ^* (sin off * realToFrac h)
    _ -> pos

mkTCMod uni pos uv m = case m of
    TM_Scroll su sv -> uv + V2 (realToFrac su) (realToFrac sv) ^* time uni
    TM_Scale su sv  -> uv * V2 (realToFrac su) (realToFrac sv)
    TM_Stretch w    -> let p    = 1 / mkWave uni w
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
                       in m !* uv + off
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

mkTexCoord uni pos normal sa uvD uvL = foldl' (mkTCMod uni pos) uv $ saTCMod sa
  where
    uv = case saTCGen sa of
        TG_Base         -> uvD
        TG_Lightmap     -> uvL
        TG_Environment  ->  let viewer      = signorm $ viewOrigin uni - pos
                                d           = normal `dot` viewer
                                V3 _ y z    = normal ^* (2 * d) - viewer
                            in V2 (0.5 + y * 0.5) (0.5 - z * 0.5)
        TG_Vector (Vec3 sx sy sz) (Vec3 tx ty tz)   -> let s    = V3 (realToFrac sx) (realToFrac sy) (realToFrac sz)
                                                           t    = V3 (realToFrac tx) (realToFrac ty) (realToFrac tz)
                                                       in V2 (pos `dot` s) (pos `dot` t)

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

entityRGB       (a,_,_,_,_,_,_) = a
entityAlpha     (_,a,_,_,_,_,_) = a
identityLight_  (_,_,a,_,_,_,_) = a
time            (_,_,_,a,_,_,_) = a
viewOrigin      (_,_,_,_,a,_,_) = a
viewTarget      (_,_,_,_,_,a,_) = a
viewUp          (_,_,_,_,_,_,a) = a
{-
uniform tuple:
      entityRGB, entityAlpha, identityLight, time,  viewOrigin, viewTarget, viewUp
      (V3 Float, Float,       Float,         Float, V3 Float,   V3 Float,   V3 Float)
-}

mkVertexShader uni ca sa (p@(V3 x y z),n,d,l,c) = (screenPos, (uv, color))
  where
    viewMat     = lookAt' (viewOrigin uni) (viewTarget uni) (viewUp uni)
    projMat     = perspective (pi/3) 1 1 10000
    screenPos   = projMat !*! viewMat !* V4 x y z 1
    pos         = foldl' (mkDeform uni d n) p $ caDeformVertexes ca
    uv          = mkTexCoord uni pos n sa d l
    color       = mkColor uni ca sa c

mkFragmentShader sa (uv,rgba) = color
  where
    stageTex    = saTexture sa
    stageTexN   = ""--TODO SB.pack $ "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
    color       = case stageTex of
        ST_WhiteImage   -> rgba
        ST_Lightmap     -> rgba * texColor ClampToEdge "LightMap"
        ST_Map {}       -> rgba * texColor Repeat  stageTexN
        ST_ClampMap {}  -> rgba * texColor ClampToEdge stageTexN
        ST_AnimMap {}   -> rgba * texColor Repeat  stageTexN
    texColor em name = V4 1 1 1 1 --TODO: texture' sampler uv
      where
        --sampler     = Sampler LinearFilter em $ TextureSlot name (Texture2D (Float RGBA) n1)

mkFilterFunction sa (uv,rgba) = case saAlphaFunc sa of
    Nothing -> true
    Just f  ->
        let
            V4 _ _ _ a  = color
            stageTex    = saTexture sa
            stageTexN   = ""--SB.pack $ "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
            color       = case stageTex of
                ST_WhiteImage   -> rgba
                ST_Lightmap     -> rgba * texColor ClampToEdge "LightMap"
                ST_Map {}       -> rgba * texColor Repeat  stageTexN
                ST_ClampMap {}  -> rgba * texColor ClampToEdge stageTexN
                ST_AnimMap {}   -> rgba * texColor Repeat  stageTexN
            texColor em name = V4 1 1 1 1-- TODO: texture' sampler uv
              where
                --sampler     = Sampler LinearFilter em $ TextureSlot name (Texture2D (Float RGBA) n1)
        in case f of
            A_Gt0   -> a >* 0
            A_Lt128 -> a GPipe.<* 0.5
            A_Ge128 -> a >=* 0.5

mkRasterContext :: CommonAttrs -> (Side, ViewPort, DepthRange)
mkRasterContext ca = (cull, ViewPort (V2 0 0) (V2 size size), DepthRange 0 1)
  where
    size = 600
    -- TODO: offset  = if caPolygonOffset ca then Offset (-1) (-2) else NoOffset
    cull    = case caCull ca of
        CT_FrontSided   -> Front
        CT_BackSided    -> Back
        CT_TwoSided     -> FrontAndBack

mkAccumulationContext :: StageAttrs -> (ContextColorOption RGBAFloat, DepthOption)
mkAccumulationContext StageAttrs{..} = (ContextColorOption blend (pure True), DepthOption depthFunc saDepthWrite)
  where
    depthFunc   = case saDepthFunc of
        D_Equal     -> Equal
        D_Lequal    -> Lequal
    blend       = case saBlend of
        Nothing     -> NoBlending
        Just (src,dst)  -> BlendRgbAlpha (FuncAdd,FuncAdd) (BlendingFactors srcF dstF, BlendingFactors srcF dstF) (V4 0 0 0 0)
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

mkStage uni ca sa = do
  primitiveStream <- toPrimitiveStream id
  fragmentStream <- rasterize (const $ mkRasterContext ca) (mkVertexShader uni ca sa <$> primitiveStream)
  let filteredFragmentStream = filterFragments (mkFilterFunction sa) fragmentStream
  drawContextColorDepth (const $ mkAccumulationContext sa) $ withRasterizedInfo (\a r -> (a, rasterizedFragCoord r ^. _z)) (mkFragmentShader sa <$> filteredFragmentStream)

mkShader uni ca = mapM_ (mkStage uni ca) $ caStages ca
