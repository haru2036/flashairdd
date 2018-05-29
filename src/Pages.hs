{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}
module Pages
    ( JSONItem
    , findWlanPush
    , matchWlanSdPush
    , parseWlanPush
    )
where
import           Text.HTML.Scalpel
import           Text.Regex.PCRE
import           Data.ByteString.Char8          ( pack )
import           Data.ByteString.Lazy           ( fromStrict )
import           Data.List.Split
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe                     ( catMaybes )

data Item = FileInfo String FilePath | DirInfo String FilePath

data JSONItem = JSONFileInfo {r_uri :: String, fname :: String, fsize :: Int, attr :: Int , fdate :: Int, ftime :: Int} deriving(Show)
$(deriveJSON defaultOptions ''JSONItem)

findWlanPush :: String -> [String]
findWlanPush str = map ((reverse . (drop 2) . reverse) . (drop 12) . matchWlanSdPush)
        $ splitOn "\n" str

parseWlanPush :: String -> Maybe JSONItem
parseWlanPush = decode . fromStrict . pack

matchWlanSdPush :: String -> String
matchWlanSdPush text = text =~ ("wlansd\\.push\\(\\{.*\\}\\);" :: String)
