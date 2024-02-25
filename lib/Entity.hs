{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies, OverloadedStrings #-}
module Entity where 
import NGraph 
import qualified Z.Data.Text as T
import Z.Data.JSON ((.:),(.=),(.!),JSON(..))
import GHC.Generics (Generic)
import qualified Z.Data.JSON as JSON
import Data.Data (Typeable)

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


