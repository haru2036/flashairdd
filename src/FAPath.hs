{-# LANGUAGE OverloadedStrings #-}
module FAPath (
    FAPath
   ,appendBaseUrl
   ,listFAFiles
   ,parseFileList
)where
import Types
import Pages
import HTTP
import           Data.ByteString                ( ByteString, writeFile)
import           Data.ByteString.Lazy           ( toStrict )
import           Data.ByteString.Char8          ( unpack )
import           Data.Maybe                     ( mapMaybe)

data FAPath = DirPath FilePath | JPGPath FilePath deriving(Show)

instance Path FAPath where
    getPath (DirPath path) = path
    getPath (JPGPath path) = path
    list p@(DirPath path) = do
        result <- getUrl $ appendBaseUrl path
        return $ map convertToFAPath $ parseFileList $ unpack result
    list p@(JPGPath path) = return [p]

parseFileList :: String -> [JSONFileInfo]
parseFileList body = mapMaybe parseWlanPush $ findWlanPush body

convertToFAPath :: JSONFileInfo -> FAPath
convertToFAPath (JSONFileInfo uri fname fsize attr fdate ftime)
    | fsize == 0 = DirPath $ uri ++ "/" ++ fname
    | otherwise = JPGPath $ uri ++ "/" ++ fname

appendBaseUrl :: String -> String
appendBaseUrl = (++) "http://192.168.1.105"

listFAFiles :: IO [FAPath]
listFAFiles = walkDir list $ DirPath "/" 

walkDir :: (FAPath -> IO [FAPath]) -> FAPath -> IO [FAPath]
walkDir list (JPGPath path) = return [JPGPath path]
walkDir list (DirPath path) = do
    paths <- list $ DirPath path
    walkd <- mapM (walkDir list) paths 
    return $ concat walkd