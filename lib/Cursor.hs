{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies , OverloadedStrings #-}
module Cursor where 

import Z.IO
import System.Process
import qualified Z.IO.FileSystem as FS
import qualified Z.Data.JSON as JSON
import Z.Data.JSON ((.:),(.=),(.!),JSON(..))
import qualified Z.Data.Text as T
import qualified Z.Data.Builder as Builder
import qualified Z.Data.CBytes as C
import GHC.Generics (Generic)

configText :: IO T.Text
configText = FS.readTextFile $ C.pack "config.json"

data Configuration = Configuration {
  address :: T.Text,
  port :: T.Text,
  user :: T.Text,
  password :: T.Text
  } deriving(Show, Generic)
    deriving anyclass (JSON.JSON)

data LoginState = LoginState {
  code :: Int,
  _data :: T.Text,
  message :: T.Text
}deriving(Show,Generic)

instance JSON.JSON LoginState where 
    fromValue = JSON.withFlatMapR "LoginState" $ \ v -> LoginState <$> v .: "code"
                  <*> v .: "data" <*> v .: "message"
    toValue (LoginState cod dat msg) = JSON.object ["code" .= cod, "data" .= dat, "message" .= msg]
    encodeJSON (LoginState cod dat msg) = JSON.object' ("code" .! cod <> "data" .! dat <> "message" .! msg)

configJSON :: IO (Maybe Configuration)
configJSON = do 
  config <- configText
  let maybe_config = JSON.decodeText' config :: Either JSON.DecodeError Configuration
  case maybe_config of
    (Right x) -> return $ Just x 
    (Left _) -> return Nothing



