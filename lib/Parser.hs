{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
module Parser where


import NGraph
import Data.List (isPrefixOf)
import Data.Typeable (cast)
import Text.Regex.PCRE
import Data.Data (Typeable)
import Timestamp (Timestamp)
import Entity




class ParserNode a where
    (→?) :: [String] -> Node a

class ParseRelation a where 
    (-→?) :: [String] -> Relation a 

parseExpr ::  String -> Expr
parseExpr x
    |  "GT" `isPrefixOf` x =  NGraph.GT $ IntVal (read (drop 2 x) :: Int)
    |  "GE" `isPrefixOf` x =  NGraph.GE $ IntVal (read (drop 2 x) :: Int)
    |  "LT" `isPrefixOf` x =  NGraph.LT $ IntVal (read (drop 2 x) :: Int)
    |  "LE" `isPrefixOf` x =  NGraph.LE $ IntVal (read (drop 2 x) :: Int)
    |  "EQ" `isPrefixOf` x =  NGraph.EQ $ IntVal (read (drop 2 x) :: Int)
    |  "REG" `isPrefixOf` x = NGraph.REG $ StringVal (drop 3 x :: String)
    |   "NULL" `isPrefixOf` x = NGraph.NULL
    | otherwise = NGraph.NULL

true :: a -> Bool
true _ = True

calculateExpr :: Expr -> Val -> Bool
calculateExpr expr  =
    case expr of
        NGraph.GT (IntVal x) -> \ (IntVal y) -> y > x
        NGraph.GE (IntVal x) -> \ (IntVal y) -> y >= x
        NGraph.LT (IntVal x) -> \ (IntVal y) -> y < x
        NGraph.LE (IntVal x) -> \ (IntVal y) -> y <= x
        NGraph.EQ (IntVal x) -> \ (IntVal y) -> y == x
        NGraph.REG (StringVal x) -> \ (StringVal y) -> y =~ x :: Bool
        NGraph.NULL -> true

anytoVal :: Typeable a => a -> Val
anytoVal a = case cast a of
                Just x -> StringVal x
                Nothing -> case cast a of
                            Just y -> IntVal y
                            




instance ParseRelation BelongTo where 
    (-→?) [] = QueryRelation $ \ belongto -> true belongto

instance ParseRelation CommentedAt where 
    (-→?) [postTime] = QueryRelation $ \ commentedat -> 
        (calculateExpr . parseExpr) postTime (TimestampVal (commentedat_post_time commentedat))

instance ParseRelation CreatedPost where 
    (-→?) [postTime] = QueryRelation $ \ createdpost ->
        (calculateExpr . parseExpr) postTime (anytoVal (createdpost_post_time createdpost))

instance ParseRelation Follow where 
    (-→?) [degree] = QueryRelation $ \ follow -> 
        (calculateExpr . parseExpr) degree (anytoVal (follow_degree follow))

instance ParseRelation LivedIn where 
    (-→?) [] = QueryRelation $ \ livedin -> true livedin

instance ParseRelation Serve where 
    (-→?) [startYear,endYear] = QueryRelation $ \ serve ->
        and
        [ (calculateExpr . parseExpr) startYear (anytoVal (serve_start_year serve)),
          (calculateExpr . parseExpr) endYear (anytoVal (serve_end_year serve))]

instance ParserNode Team where
    (→?) [teamName] = QueryNode $ \team ->
        (calculateExpr . parseExpr) teamName (anytoVal (team_name team))

instance ParserNode Place where
    (→?) [placeName,placeGeoPoint] =
        QueryNode $ \place ->
             and
             [(calculateExpr . parseExpr) placeName (anytoVal (place_name place)),
              (calculateExpr . parseExpr) placeGeoPoint (anytoVal (place_geo_point place))]

instance ParserNode Player where
    (→?) [playerName,playerAge] =
        QueryNode $ \player ->
            and
            [(calculateExpr . parseExpr) playerName (anytoVal (player_name player)),
             (calculateExpr . parseExpr) playerAge (anytoVal (player_age player))]

instance ParserNode Post where
    (→?) [postTitle] =
        QueryNode $ \post ->
            (calculateExpr . parseExpr) postTitle (anytoVal (post_title post))

instance ParserNode Address where
    (→?) [addressAddress,addressGeoPoint] =
        QueryNode $ \address ->
            and
            [(calculateExpr . parseExpr) addressAddress (anytoVal (address_address address)),
             (calculateExpr . parseExpr) addressGeoPoint (anytoVal (address_geo_point address))]





