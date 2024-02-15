module Main where

import Cursor 
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL as URL
import Control.Exception(try, SomeException)
import Codec.Binary.UTF8.String 
import System.FilePath(takeExtension)
import Server (runServer)

main :: IO ()
main = runServer

