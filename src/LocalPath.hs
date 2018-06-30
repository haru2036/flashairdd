{-# LANGUAGE OverloadedStrings #-}
module LocalPath
    ( LocalPath
    , listLocalFiles
    )
where

import           Types
import           System.Directory

data LocalPath = LDirPath FilePath | LJPGPath FilePath deriving(Show)

instance Path LocalPath where
    getPath (LDirPath path) = path
    getPath (LJPGPath path) = path
    list (LDirPath path) = do
        canonicalPath <- canonicalizePath path
        directories <- listDirectory canonicalPath
        mapM (fromFilePath . (++) (canonicalPath ++ "/")) directories
    list p@(LJPGPath path) = return [p]
    walkDir list (LJPGPath path) = return [LJPGPath path]
    walkDir list (LDirPath path) = do
        paths <- list $ LDirPath path
        walkd <- mapM (walkDir list) paths
        return $ concat walkd

listLocalFiles :: IO [LocalPath]
listLocalFiles = do
    cd <- getCurrentDirectory
    walkDir list $ LDirPath $ cd ++ "/photos"


fromFilePath :: FilePath -> IO LocalPath
fromFilePath path = do
    -- TODO 設定にしたがって保存先のパスを付け足すようにする
    isdir <- doesDirectoryExist $ path
    return $ case isdir of
        True  -> LDirPath path
        False -> LJPGPath path
