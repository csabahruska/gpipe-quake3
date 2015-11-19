{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections #-}

import Text.Show.Pretty
import Data.Attoparsec.ByteString.Char8

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

    --putStrLn "material list:"
    --putStrLn $ unlines ["  " ++ l | l <- lines $ ppShow $ T.toList shMap]

    putStrLn $ "loading: " ++ show bspName
    renderQuake p0 bsp

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
