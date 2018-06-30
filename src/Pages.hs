{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}
module Pages where
import           Text.Regex.PCRE
import           Data.ByteString.Char8          ( pack )
import           Data.ByteString.Lazy           ( fromStrict )
import           Data.List.Split
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe                     ( catMaybes )

data Item = FileInfo String FilePath | DirInfo String FilePath

data JSONFileInfo = JSONFileInfo {r_uri :: String, fname :: String, fsize :: Int, attr :: Int , fdate :: Int, ftime :: Int} deriving(Show)
$(deriveJSON defaultOptions ''JSONFileInfo)

findWlanPush :: String -> [String]
findWlanPush str =
        map ((reverse . (drop 2) . reverse) . (drop 12) . matchWlanSdPush)
                $ splitOn "\n" str

parseWlanPush :: String -> Maybe JSONFileInfo
parseWlanPush = decode . fromStrict . pack

matchWlanSdPush :: String -> String
matchWlanSdPush text = text =~ ("wlansd\\.push\\(\\{.*\\}\\);" :: String)
