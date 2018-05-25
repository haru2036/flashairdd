{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell#-}
module Pages
    ( 
     submittedItem 
    ,findWlanPush
    ,matchWlanSdPush
    ) where
import Text.HTML.Scalpel
import Text.Regex.PCRE
import Data.List.Split
import Data.Aeson
import Data.Aeson.TH

data Item = FileInfo String FilePath | DirInfo String FilePath

data JSONItem = JSONFileInfo {r_uri :: String, fname :: String, fsize :: String, attr :: Int , fdate :: Int, ftime :: Int} deriving(Show)
$(deriveJSON defaultOptions ''JSONItem) 

--wlanPushRegex = mkRegex "wlansd.push\\(\\{.*\\}\\);\n"

submittedItem :: Scraper String [String]
submittedItem = texts $ "script" @: []

findWlanPush :: String -> Maybe [String]
findWlanPush str = do
    Just $ map (( reverse . (drop 2) . reverse) . (drop 12) . matchWlanSdPush ) $ splitOn "\n" str
   

matchWlanSdPush :: String -> String
matchWlanSdPush text = text =~ ("wlansd\\.push\\(\\{.*\\}\\);" :: String) 
