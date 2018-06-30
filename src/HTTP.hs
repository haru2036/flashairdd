module HTTP where

import           Data.ByteString                ( ByteString
                                                , writeFile
                                                )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.ByteString.Char8          ( unpack )
import           Network.HTTP.Client

getUrl :: String -> IO ByteString
getUrl url = do
    manager  <- newManager defaultManagerSettings
    request  <- parseRequest url
    response <- httpLbs request manager
    return $ toStrict $ responseBody response
