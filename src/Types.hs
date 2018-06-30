module Types where

class Path a where
    getPath :: a -> FilePath
    walkDir :: (a -> IO [a]) -> a -> IO [a]
    list :: a -> IO [a]
