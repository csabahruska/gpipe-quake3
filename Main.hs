{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections #-}

import Text.Show.Pretty
import Data.Attoparsec.ByteString.Char8

import Data.Maybe
import Data.Char
import Data.List (isPrefixOf,partition)
import System.Directory
import System.Environment
import System.FilePath
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.Trie as T
import qualified Data.Vector as V
import Data.Vect hiding (Vector)
import Codec.Picture

import BSP
import ShaderParser
import Material
import Zip
import Render

main :: IO ()
main = do
    ar <- loadArchive

    let imageShader txName = defaultCommonAttrs {caStages = sa:saLM:[]}
          where
            sa = defaultStageAttrs
                { saTexture     = ST_Map txName
                , saBlend       = Nothing
                , saTCGen       = TG_Base
                , saDepthWrite  = True
                , saRGBGen      = RGB_IdentityLighting
                }
            saLM = defaultStageAttrs
                { saTexture = ST_Lightmap
                , saBlend   = Just (B_DstColor,B_Zero)
                , saTCGen   = TG_Lightmap
                , saRGBGen  = RGB_IdentityLighting
                }

    args <- getArgs
    let bspMap = T.fromList [(SB.pack $ takeBaseName n, decompress' e) | e <- ar, let n = eFilePath e, ".bsp" == takeExtensionCI n, isPrefixOfCI "maps" n]
        bspName = case args of
            []     -> head $ T.keys bspMap
            (n:xs) -> SB.pack n
        bspData = case T.lookup bspName bspMap of
            Nothing -> error "You need to put pk3 file into your current directory"
            Just bspd -> bspd
        bsp = readBSP bspData
        shNames = Set.fromList $ map shName $ V.toList $ blShaders bsp
        shMap' = shaderMap ar
        (normalShNames,textureShNames) = partition (\n -> T.member n shMap') $ Set.toList shNames
        normalShNameSet     = Set.fromList normalShNames
        textureShNameSet    = Set.fromList textureShNames
        normalShMap     = T.mapBy (\n sh -> if Set.member n normalShNameSet then Just sh else Nothing) shMap'
        --textureShMap    = T.fromList [(n,defaultCommonAttrs {caStages = [defaultStageAttrs {saTexture = ST_Map n, saDepthWrite = True}]}) | n <- Set.toList textureShNameSet]
        textureShMap    = T.fromList [(n,imageShader n) | n <- Set.toList textureShNameSet]
        shMap = T.unionL normalShMap textureShMap

        -- extract spawn points
        ents = parseEntities (SB.unpack bspName) $ blEntities bsp
        spawn e = case T.lookup "classname" e of
            Just "info_player_deathmatch"   -> True
            Just "info_player_start"        -> True
            Just "info_player_intermission" -> True
            _                               -> False
        Just sp0 = T.lookup "origin" $ head $ filter spawn ents
        [x0,y0,z0] = map read $ words $ SB.unpack sp0 :: [Float]
        p0 = Vec3 x0 y0 z0

        -- load textures
        textureSet  = foldMap (mconcat . map stageTex . caStages) shMap
        stageTex sa = case saTexture sa of
          ST_Map n        -> Set.singleton n
          ST_ClampMap n   -> Set.singleton n
          ST_AnimMap _ l  -> mconcat $ map Set.singleton l
          _ -> mempty
        archiveTrie = T.fromList [(SB.pack $ eFilePath a,a) | a <- ar]
        textureMap  = T.fromList [(n, i) | n <- Set.toList textureSet, i <- maybeToList $ loadQ3Texture archiveTrie n]
{-
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ map (\(s,m) -> (saTexture s,m)) $
               concatMap (\sh -> [(s,caNoMipMaps sh) | s <- caStages sh]) $ T.elems shMap) $ \(stageTex,noMip) -> do
        let texSlotName = SB.pack $ "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
            setTex isClamped img  = uniformFTexture2D texSlotName slotU =<< loadQ3Texture (not noMip) isClamped defaultTexture archiveTrie img
        case stageTex of
            ST_Map img          -> setTex False img >> return []
            ST_ClampMap img     -> setTex True img >> return []
            ST_AnimMap t imgs   -> do
                txList <- mapM (loadQ3Texture (not noMip) False defaultTexture archiveTrie) imgs
                --return [(1 / t / fromIntegral (length imgs),cycle $ zip (repeat (uniformFTexture2D texSlotName slotU)) txList)]
                return [(1/t,cycle $ zip (repeat (uniformFTexture2D texSlotName slotU)) txList)]
            _ -> return []
-}
    putStrLn $ "loading: " ++ show bspName
    renderQuake p0 bsp shMap textureMap

-- pk3 handling

takeExtensionCI = map toLower . takeExtension
isPrefixOfCI a b = isPrefixOf a $ map toLower b

loadArchive :: IO Archive
loadArchive = concat <$> (mapM readArchive =<< filter (\n -> ".pk3" == takeExtensionCI n) <$> getDirectoryContents ".")

shaderMap :: Archive -> T.Trie CommonAttrs
shaderMap ar = T.fromList $ concat [eval n $ parse shaders d | (n,d) <- l]
  where
    l = [(n,decompress e) | e <- ar, let n = eFilePath e, ".shader" == takeExtensionCI n, isPrefixOfCI "scripts" n]
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, map fst r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")

parseEntities :: String -> SB.ByteString -> [T.Trie SB.ByteString]
parseEntities n s = eval n $ parse entities s
  where
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")

loadQ3Texture :: T.Trie Entry -> SB.ByteString -> Maybe DynamicImage
loadQ3Texture ar name = case T.lookup fname ar of
  Nothing -> Nothing
  Just d  -> case decodeImage $ decompress d of
    Left msg  -> Nothing
    Right img -> Just img
 where
  name' = SB.unpack name
  n1 = SB.pack $ replaceExtension name' "tga"
  n2 = SB.pack $ replaceExtension name' "jpg"
  b0 = T.member name ar
  b1 = T.member n1 ar
  b2 = T.member n2 ar
  fname   = if b0 then name else if b1 then n1 else n2
