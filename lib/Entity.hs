{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies, OverloadedStrings #-}
module Entity where 
import NGraph 
import qualified Z.Data.Text as T
import qualified Z.Data.JSON as JSON
import Z.Data.JSON ((.:),(.=),(.!),JSON(..))
import GHC.Generics (Generic)

-- finish test
data Player = Player {
    player_name :: T.Text, 
    player_age :: Int
    } deriving(Show, Generic)

instance JSON.JSON Player where 
    fromValue = JSON.withFlatMapR "Player" $ \ v -> Player <$> v .: "name"
                   <*> v .: "age"
    toValue (Player playerName playerAge) = JSON.object ["name" .= playerName, "age" .= playerAge]
    encodeJSON (Player playerName playerAge) = JSON.object' ("name" .! playerName <> "age" .! playerAge)

data Team = Team {
    team_name :: T.Text 
    } deriving(Show, Generic)

instance JSON.JSON Team where 
    fromValue = JSON.withFlatMapR "Team" $ \ v -> Team <$> v .: "name"
    toValue (Team teamName) = JSON.object ["name" .= teamName]
    encodeJSON (Team teamName) = JSON.object' ("name" .! teamName)

