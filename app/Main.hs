{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Cursor
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL as URL
import Control.Exception(try, SomeException)
import Codec.Binary.UTF8.String
import System.FilePath(takeExtension)
import qualified Z.Data.JSON as JSON
import Network.HTTP.Client
import Data.ByteString.Lazy.Internal as I
import qualified Z.Data.Text as T
import Network.HTTP.Types
import GHC.Generics (Generic)
import Z.Data.JSON ((.:),(.=),(.!),JSON(..))
import qualified Z.Data.JSON.Base as JSON
import Data.Data (Typeable)
import Data.Typeable (cast)
import Data.Typeable (gcast)
import Text.Regex.PCRE
import Server (gatewayLogin, runServer, gatewaySchemaQuery, gatewayConnectedQuery_1, gatewayShortestPATHQuery)
import Entity (LivedIn(LivedIn), CommentedAt (CommentedAt))



main :: IO ()
main =
    gatewayLogin
    >>= runServer 




