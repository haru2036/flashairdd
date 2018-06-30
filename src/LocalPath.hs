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

listLocalFiles :: IO [LocalPath]
listLocalFiles = do
    cd <- getCurrentDirectory
    walkDirLocal list $ LDirPath $ cd ++ "/photos"

walkDirLocal :: (LocalPath -> IO [LocalPath]) -> LocalPath -> IO [LocalPath]
walkDirLocal list (LJPGPath path) = return [LJPGPath path]
walkDirLocal list (LDirPath path) = do
    paths <- list $ LDirPath path
    walkd <- mapM (walkDirLocal list) paths
    print walkd
    return $ concat walkd

isDirectoryLocal :: FilePath -> IO LocalPath
isDirectoryLocal path = do
    print path
    isdir <- doesDirectoryExist path
    return $ case isdir of
        True  -> LDirPath path
        False -> LJPGPath path
