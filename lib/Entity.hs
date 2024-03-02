{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Entity where 
import NGraph 
import qualified Z.Data.Text as T
import Z.Data.JSON ((.:),(.=),(.!),JSON(..))
import GHC.Generics (Generic)
import qualified Z.Data.JSON as JSON
import Data.Data (Typeable)
import Timestamp

data Team = Team {
    team_name :: String
} deriving (Show,Generic,Typeable,Eq)

data Place = Place {
    place_name :: String,
    place_geo_point :: String  
} deriving (Show,Generic,Typeable,Eq)

data Player = Player {
    player_name :: String, 
    player_age :: Int
} deriving(Show, Generic,Typeable,Eq)

data Post = Post {
    post_title :: String 
} deriving(Show,Generic,Typeable,Eq)

data Address = Address {
    address_address :: String,
    address_geo_point :: String
} deriving(Show,Generic,Typeable,Eq)

data BelongTo = BelongTo deriving (Show,Generic,Typeable,Eq)
    deriving anyclass (JSON.JSON)

data CommentedAt = CommentedAt {
    commentedat_post_time :: Timestamp
}deriving (Show,Generic,Typeable,Eq)

data CreatedPost = CreatedPost {
    createdpost_post_time :: Timestamp
} deriving (Show,Generic,Typeable,Eq)

data Follow = Follow {
    follow_degree :: Int 
} deriving (Show,Generic,Typeable,Eq)

data LivedIn = LivedIn deriving (Show,Generic,Typeable,Eq)
    deriving anyclass (JSON.JSON)

data Serve = Serve {
    serve_start_year :: Int,
    serve_end_year :: Int
} deriving (Show,Generic,Typeable,Eq)

instance JSON.JSON CreatedPost where 
    fromValue = JSON.withFlatMapR "CreatedPost" $ \ v -> CreatedPost <$> v .: "post_time"
    toValue (CreatedPost postTime) = JSON.object ["post_time" .= postTime]
    encodeJSON (CreatedPost postTime) = JSON.object' ("name" .! postTime) 

instance JSON.JSON Follow where 
    fromValue = JSON.withFlatMapR "Follow" $ \ v -> Follow <$> v .: "degree"
    toValue (Follow degree) = JSON.object ["degree" .= degree]
    encodeJSON (Follow degree) = JSON.object' ("degree" .! degree)

instance JSON.JSON Serve where 
    fromValue = JSON.withFlatMapR "Serve" $ \ v -> Serve <$> v .: "start_year" 
                    <*> v .: "end_year"
    toValue (Serve start_year end_year) = JSON.object ["start_year" .= start_year,"end_year" .= end_year]
    encodeJSON (Serve start_year end_year) = JSON.object' ("start_year" .! start_year <> "end_year" .! end_year)    

instance JSON.JSON Timestamp 

instance JSON.JSON CommentedAt where 
    fromValue = JSON.withFlatMapR "CommentedAt" $ \v ->  CommentedAt <$> v .: "post_time"
    toValue (CommentedAt postTime) = JSON.object ["post_time" .= postTime]
    encodeJSON (CommentedAt postTime) = JSON.object' ("name" .! postTime)

instance JSON.JSON Player where 
    fromValue = JSON.withFlatMapR "Player" $ \ v -> Player <$> v .: "name"
                   <*> v .: "age"
    toValue (Player playerName playerAge) = JSON.object ["name" .= playerName, "age" .= playerAge]
    encodeJSON (Player playerName playerAge) = JSON.object' ("name" .! playerName <> "age" .! playerAge)

instance JSON.JSON Team where 
    fromValue = JSON.withFlatMapR "Team" $ \ v -> Team <$> v .: "name"
    toValue (Team teamName) = JSON.object ["name" .= teamName]
    encodeJSON (Team teamName) = JSON.object' ("name" .! teamName)

instance JSON.JSON Place where 
    fromValue = JSON.withFlatMapR "Place" $ \ v -> Place <$> v .: "name"
                    <*> v .: "geo_point"
    toValue (Place placeName placeGeoPoint) = JSON.object ["name" .= placeName , "geo_point" .= placeGeoPoint]
    encodeJSON (Place placeName placeGeoPoint) = JSON.object' ("name" .! placeName <> "geo_point" .! placeGeoPoint)

instance JSON.JSON Post where 
    fromValue = JSON.withFlatMapR "Post" $ \ v -> Post <$> v .: "title"
    toValue (Post postTitle) = JSON.object ["title" .= postTitle]
    encodeJSON (Post postTitle) = JSON.object' ("title" .! postTitle)

instance JSON.JSON Address where 
    fromValue = JSON.withFlatMapR "Address" $ \ v -> Address <$> v .: "address"
                    <*> v .: "geo_point"
    toValue (Address addressAddress addressGeoPoint) = JSON.object ["address" .= addressAddress, "geo_point" .= addressGeoPoint]
    encodeJSON (Address addressAddress addresssGeoPoint) = JSON.object' ("address" .! addressAddress <> "geo_point" .! addresssGeoPoint)
