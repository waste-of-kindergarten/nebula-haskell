{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies , OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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
import Text.Regex.PCRE
import Entity
import NGraph
import Timestamp
import Entity (CommentedAt(CommentedAt), CreatedPost (CreatedPost))

configText :: IO T.Text
configText = FS.readTextFile $ C.pack "config.json"

{-
data Configuration = Configuration {
  address :: T.Text,
  port :: T.Text,
  user :: T.Text,
  password :: T.Text
  } deriving(Show, Generic)
    deriving anyclass (JSON.JSON)
-}
data Configuration = Configuration {
  address :: String,
  port :: String,
  user :: String,
  password :: String
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

regexfunc :: String -> String -> (String,String)
regexfunc str rule = (curr,rema)
    where (_ ,curr,rema) = str =~ rule :: (String,String,String)

generate :: String -> String -> String -> [String]
generate name rule str
    | curr /= "" = drop (length name + 3) curr:generate name rule rema
    | otherwise = []
      where (curr,rema) = regexfunc str rule

generateVid :: String -> [String]
generateVid str
    | curr /= "" = drop 6 curr:generateVid rema
    | otherwise = []
      where (curr,rema) = regexfunc str "\"vid\":\\s\"(.*?)\""

generateSrcVid :: String -> [String]
generateSrcVid str 
  | curr /= "" = drop 8 curr:generateSrcVid rema 
  | otherwise = []
    where (curr,rema) = regexfunc str "\"srcID\":\\s\"(.*?)\""

generateDstVid :: String -> [String]
generateDstVid str 
  | curr /= "" = drop 8 curr:generateDstVid rema 
  | otherwise = []
    where (curr,rema) = regexfunc str "\"dstID\":\\s\"(.*?)\""

class Generable a where
  (→:) :: String -> [Node a]
  sign :: a -> String
  rule :: a -> String
  static :: a

class Generable' a where 
  (-→:) :: String -> [Relation a]
  sign' :: a -> String 
  sign' = const "properties"
  rule' :: a -> String 
  rule' = const "\"properties\":\\s\\{[\\s\\S]*?\\}"
  static' :: a 
  src' :: a -> String 
  src' = const "\"srcID\":\\s\"[\\s\\S]*?\""
  dst' :: a -> String
  dst' = const "\"dstID\":\\s\"[\\s\\S]*?\"" 


instance Generable Team where
  sign = const "team"
  rule = const "\"team\":\\s\\{([\\s\\S]*?)\\}"
  static = Team ""
  (→:) x =  fmap Node (helper strls)
    where strls = generate (sign (static :: Team)) (rule (static :: Team)) x
          helper strls = case strls of
                    [] -> []
                    s:ss -> case (JSON.decodeText' (T.pack s) :: Either JSON.DecodeError Team) of
                      Left _ -> []
                      Right r -> r : helper ss

instance Generable Place where
  sign = const "place"
  rule = const "\"place\":\\s\\{([\\s\\S]*?)\\}"
  static = Place "" ""
  (→:) x = fmap Node (helper strls)
    where strls = generate (sign (static :: Place)) (rule (static :: Place)) x
          helper strls = case strls of
                      [] -> []
                      s:ss -> case (JSON.decodeText' (T.pack s) :: Either JSON.DecodeError Place) of
                        Left _ -> []
                        Right r -> r : helper ss

instance Generable Player where
  sign = const "player"
  rule = const "\"player\":\\s\\{([\\s\\S]*?)\\}"
  static = Player "" 0
  (→:) x = fmap Node (helper strls)
    where strls = generate (sign (static :: Player)) (rule (static :: Player)) x
          helper strls = case strls of
                      [] -> []
                      s:ss -> case (JSON.decodeText' (T.pack s) :: Either JSON.DecodeError Player) of
                        Left _ -> []
                        Right r -> r : helper ss


instance Generable Post where
  sign = const "post"
  rule = const "\"post\":\\s\\{([\\s\\S]*?)\\}"
  static = Post ""
  (→:) x = fmap Node (helper strls)
    where strls = generate (sign (static :: Post)) (rule (static :: Post)) x
          helper strls = case strls of
                      [] -> []
                      s:ss -> case (JSON.decodeText' (T.pack s) :: Either JSON.DecodeError Post) of
                        Left _ -> []
                        Right r -> r : helper ss


instance Generable Address where
  sign = const "address"
  rule = const "\"address\":\\s\\{([\\s\\S]*?)\\}"
  static = Address "" ""
  (→:) x = fmap Node (helper strls)
    where strls = generate (sign (static :: Address)) (rule (static :: Address)) x
          helper strls = case strls of
                      [] -> []
                      s:ss -> case (JSON.decodeText' (T.pack s) :: Either JSON.DecodeError Address) of
                        Left _ -> []
                        Right r -> r : helper ss

instance Generable' BelongTo where
  static' = BelongTo 
  (-→:) x = fmap Relation (helper strls)
    where strls = generate (sign' (static' :: BelongTo)) (rule' (static' :: BelongTo)) x
          helper strls = case strls of
                          [] -> []
                          _:ss -> BelongTo : helper ss

instance Generable' CommentedAt where
  static' = CommentedAt (Timestamp 0)
  (-→:) x = fmap Relation (helper strls)
    where strls = generate (sign' (static' :: CommentedAt)) (rule' (static' :: CommentedAt)) x 
          helper strls = case strls of 
                          [] -> []
                          s:ss -> case (JSON.decodeText' (T.pack s) :: Either JSON.DecodeError CommentedAt) of
                                    Left _ -> []
                                    Right r -> r : helper ss 

instance Generable' CreatedPost where 
  static' = CreatedPost (Timestamp 0)
  (-→:) x = fmap Relation (helper strls)
      where strls = generate (sign' (static' :: CreatedPost)) (rule' (static' :: CreatedPost)) x 
            helper strls = case strls of 
                          [] -> []
                          s:ss -> case (JSON.decodeText' (T.pack s) :: Either JSON.DecodeError CreatedPost) of
                                    Left _ -> []
                                    Right r -> r : helper ss

instance Generable' Follow where 
  static' = Follow 0
  (-→:) x = fmap Relation (helper strls)
      where strls = generate (sign' (static' :: Follow)) (rule' (static' :: Follow)) x 
            helper strls = case strls of 
                          [] -> []
                          s:ss -> case (JSON.decodeText' (T.pack s) :: Either JSON.DecodeError Follow) of
                                    Left _ -> []
                                    Right r -> r : helper ss


instance Generable' LivedIn where 
    static' = LivedIn
    (-→:) x = fmap Relation (helper strls)
      where strls = generate (sign' (static' :: LivedIn)) (rule' (static' :: LivedIn)) x 
            helper strls = case strls of 
                          [] -> []
                          _:ss -> LivedIn : helper ss

instance Generable' Serve where 
    static' = Serve 0 0 
    (-→:) x = fmap Relation (helper strls)
      where strls = generate (sign' (static' :: Serve)) (rule' (static' :: Serve)) x 
            helper strls = case strls of 
                          [] -> []
                          s:ss -> case (JSON.decodeText' (T.pack s) :: Either JSON.DecodeError Serve) of
                                    Left _ -> []
                                    Right r -> r : helper ss

