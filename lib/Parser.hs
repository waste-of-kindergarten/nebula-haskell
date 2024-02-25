{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
module Parser where

import Entity
import NGraph
import Data.List (isPrefixOf)
import Data.Typeable (cast)
import Text.Regex.PCRE
import Data.Data (Typeable)


class ParserNode a where
    (→?) :: [String] -> Node a

parseExpr ::  String -> Expr
parseExpr x
    | x `isPrefixOf` "GT" =  NGraph.GT $ IntVal (read (drop 2 x) :: Int)
    | x `isPrefixOf` "GE" =  NGraph.GE $ IntVal (read (drop 2 x) :: Int)
    | x `isPrefixOf` "LT" =  NGraph.LT $ IntVal (read (drop 2 x) :: Int)
    | x `isPrefixOf` "LE" =  NGraph.LE $ IntVal (read (drop 2 x) :: Int)
    | x `isPrefixOf` "EQ" =  NGraph.EQ $ IntVal (read (drop 2 x) :: Int)
    | x `isPrefixOf` "REG" = NGraph.REG $ StringVal (read (drop 2 x) :: String)
    | x `isPrefixOf` "NULL" = NGraph.NULL

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
        NGraph.REG (StringVal x) -> \ (StringVal y) -> x =~ y :: Bool
        NGraph.NULL -> true

anytoVal :: Typeable a => a -> Val
anytoVal a = case cast a of
                Just x -> StringVal x
                Nothing -> case cast a of
                            Just y -> IntVal y

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





