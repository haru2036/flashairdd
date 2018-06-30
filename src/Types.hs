module Types where

class Path a where
    getPath :: a -> FilePath
    list :: a -> IO [a]
