module Server where

import Network.HTTP.Server
import Network.HTTP.Server.Logger 
import Network.URL as URL 
import Control.Exception(try, SomeException)
import Codec.Binary.UTF8.String  
import Data.List (isPrefixOf)
import qualified Z.Data.JSON as JSON
import qualified Z.Data.Text as T

runServer :: IO () 
runServer = serverWith defaultConfig {srvLog = stdLogger, srvPort = 10000 }
    $ \_ url request -> 
        case rqMethod request of 
            POST -> 
                return $ 
                    case findHeader HdrContentType request of 
                        Just ty | "application/json" `isPrefixOf` ty -> sendText OK (decodeString (rqBody request))
                        _ -> sendText BadRequest "Unresolved content"
            _ -> return $ sendText BadRequest "disallowed method"

sendText       :: StatusCode -> String -> Response String
sendText s v    = insertHeader HdrContentLength (show (length txt))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentEncoding "text/plain"
                $ (respond s :: Response String) { rspBody = txt }
  where txt       = encodeString v

sendJSON :: (JSON.JSON a) => StatusCode -> a -> Response String 
sendJSON s v = insertHeader HdrContentType "application/json"
    $ sendText s (T.unpack $ JSON.encodeText v) 
