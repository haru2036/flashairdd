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
        directories <- listDirectory path
        mapM isDirectoryLocal directories
    list p@(LJPGPath path) = return [p]
    walkDir list (LJPGPath path) = return [LJPGPath path]
    walkDir list (LDirPath path) = do
        paths <- list $ LDirPath path
        walkd <- mapM (walkDir list) paths
        print walkd
        return $ concat walkd

listLocalFiles :: IO [LocalPath]
listLocalFiles = do
    cd <- getCurrentDirectory
    walkDir list $ LDirPath $ cd ++ "/photos"


isDirectoryLocal :: FilePath -> IO LocalPath
isDirectoryLocal path = do
    print path
    isdir <- doesDirectoryExist path
    return $ case isdir of
        True  -> LDirPath path
        False -> LJPGPath path
