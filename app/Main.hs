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
import Server (gatewayLogin, runServer)


{-
data Data = Data {
    headers :: [T.Text] ,
    tables :: [T.Text],
    timeCost :: Int,
    localParams :: T.Text 
} deriving(Show,Generic)
  deriving anyclass (JSON.JSON)

data Resp = Resp {
    code :: T.Text,
    _data ::  Data,
    message :: T.Text 
} deriving(Show,Generic)

instance JSON.JSON Resp where 
    fromValue = JSON.withFlatMapR "Resp" $ \ v -> Resp <$> v .: "code" <*> v .: "data" <*> v .: "message"
    toValue (Resp cod dat mes) = JSON.object ["code" .= cod , "data" .= dat , "message" .= mes]
    encodeJSON (Resp cod dat mes) = JSON.object' ("code" .! cod <> "data" .! dat <> "message" .! mes)  

data Club = Club {
    name :: T.Text 
} deriving(Show,Generic)
  deriving anyclass (JSON.JSON)

data P = forall a.(Show a,Typeable a) => P a

data V1 = V1 deriving (Typeable,Show)

data V2 = V2 deriving (Typeable,Show)

a :: P
a = P V1 

f1 :: V1 -> Int 
f1 _ = 1 
f2 :: V2 -> Int 
f2 _ = 2 

f :: P -> Int
f (P x) = case cast x of 
        Just x -> f1 x 
        Nothing -> case cast x of 
                    Just x -> f2 x 
                    Nothing -> 0 


-}

{-
toEntity :: [String] -> [Team]
toEntity ls = foldr fun [] templist
    where
    templist :: [Either JSON.DecodeError Team]
    templist = fmap (JSON.decodeText' . T.pack) ls
    fun :: Either JSON.DecodeError Team -> [Team] -> [Team]
    fun x xs = case x of 
                Right r -> r : xs 
                Left _ -> xs 
-}

main :: IO ()
main =  do 
    gatewayLogin 
    >>= runServer 

{-
main :: IO ()
main =
    do
        manager <- newManager defaultManagerSettings
        initialRequest <- parseRequest "http://127.0.0.1:8080/api/db/exec"
        let request = initialRequest {method = "POST",requestHeaders = [(hCookie,"SameSite=None; common-nsid=c22dd41fe96be1d23d50b02cbbbac7f1;")],requestBody = RequestBodyLBS "{\"gql\":\"use  demo_football_2022;match  p=(v1 : player)--(v2 : team) return v2 limit 2\"}"}
        response <- httpLbs request manager
        let x = responseBody response
        let x1 = I.unpackChars x
        --putStrLn x1
        --let (pre,cur,remain) =  (x1 :: String) =~ ("\"team\":\\s\\{([\\s\\S]*?)\\}" :: String) :: (String,String,String)
        print $ toEntity (generate "\"team\":\\s\\{([\\s\\S]*?)\\}" x1)
-}
{-
let y = JSON.decodeText' (T.pack x1) :: Either JSON.DecodeError Resp
case y of 
    (Right z) -> print z 
    (Left _) -> print "cannnot parse"
    -}
{-let y = JSON.decodeText' (T.pack x1) :: Either JSON.DecodeError LoginState
case y of 
    (Right z) -> print z 
    (Left _) -> print "cannot parse"
-}

    {-do 
    manager <- newManager defaultManagerSettings 
    request <- parseRequest "http://127.0.0.1:8080"
    response <- httpLbs request manager 
    print $ responseBody response
    -}

