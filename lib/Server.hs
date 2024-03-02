{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Server where

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL as URL
import Control.Exception(try, SomeException)
import Codec.Binary.UTF8.String
import Data.List (isPrefixOf)
import qualified Z.Data.JSON as JSON
import qualified Z.Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client
import Data.ByteString.Internal as I
import Data.ByteString.Lazy.Internal as I'
import Text.Regex.PCRE
import Network.HTTP.Types (hCookie)
import Entity
import Cursor
import Parser ((→?), calculateExpr, parseExpr, anytoVal, ParseRelation ((-→?)))
import NGraph ((⇛), Node (Node), Expr (REG), Val (StringVal), Relation (Relation))
import qualified Control.Applicative as GHC.Base

runServer :: String -> IO ()
runServer cookie =
    serverWith defaultConfig {srvLog = stdLogger, srvPort = 10000 }
    $ \_ url request ->
        case rqMethod request of
            POST ->
                    case findHeader HdrContentType request of
                        Just ty | "application/json" `isPrefixOf` ty ->
                                    serveFunction cookie (rqBody request)
                                    -- sendText OK (rqBody request)
                        _ -> return $ sendText BadRequest "Unresolved content"
            _ -> return $ sendText BadRequest "disallowed method"

sendText       :: StatusCode -> String -> Network.HTTP.Server.Response String
sendText s v    = insertHeader HdrContentLength (show (length txt))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentEncoding "text/plain"
                $ (respond s :: Network.HTTP.Server.Response String) { rspBody = txt }
  where txt       = encodeString v

sendJSON :: (JSON.JSON a) => StatusCode -> a -> Network.HTTP.Server.Response String
sendJSON s v = insertHeader HdrContentType "application/json"
    $ sendText s (T.unpack $ JSON.encodeText v)

data Query = Query {
    query_type :: String,
    query_body :: QueryBody
} deriving(Show,Generic)
    deriving anyclass (JSON.JSON)

data QueryBody = NodeQuery {
    body_name :: String,
    body_query :: [String]
} deriving(Show,Generic)
    deriving anyclass (JSON.JSON)


serveFunction :: String -> String -> IO (Network.HTTP.Server.Response String)
serveFunction cookie query = case _query of
        Left _ -> return $ sendText OK ("internal error: 1"  <> query)
        Right q | query_type q == "tags" -> do
                                            ls <- gatewayQuery cookie (body_query . query_body $ q) (body_name . query_body $ q)
                                            --print _query
                                            return $ sendJSON OK ls
        Right q | query_type q == "edges" -> do
                                            ls <- gatewayQuery' cookie (body_query . query_body $ q) (body_name . query_body $ q)
                                            idls <- gatewayVidQuery cookie ([ t  | (t,_,_) <- ls] <> [ t | (_,t,_) <- ls])
                                            let rells = fmap (\(_,_,c) -> c) ls 
                                            print idls 
                                            --print rells
                                            return $ sendJSON OK [[ t  | (t,_,_) <- ls],[ t | (_,t,_) <- ls],idls,rells]
    where _query = JSON.decodeText' (T.pack query) :: Either JSON.DecodeError Query
         {- _node_query  :: Either JSON.DecodeError NodeQuery
          _node_query = case _query of 
                            Right x -> JSON.decodeText' (T.pack (query_body x)) :: Either JSON.DecodeError NodeQuery
                            Left t -> Left t -}




gatewayLogin :: IO String
gatewayLogin = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest "http://127.0.0.1:8080/api/db/connect"
    let request = initialRequest {method = "POST", requestBody = "{\"username\":\"root\",\"address\":\"192.168.42.141\",\"port\":9669}"}
    response <- httpLbs request manager
    let x = ((I.unpackChars . snd) (responseHeaders response !! 3) :: String) =~ ("common-nsid=(.*?);" :: String) :: String
    return $ "SameSite=None;" <> x

gatewayQuery' :: String -> [String] -> String -> IO [(String,String,String)]
gatewayQuery' cookie query tag = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest "http://127.0.0.1:8080/api/db/exec"
    let request = initialRequest {method = "POST",requestHeaders = [(hCookie,I.packChars cookie)],requestBody = RequestBodyLBS "{\"gql\":\"use demo_sns; match ()-[e:" <> (RequestBodyLBS . I'.packChars) tag <> "]->() return e limit 100\"}"}
    response <- httpLbs request manager
    let x = (I'.unpackChars . responseBody) response
    putStrLn x
    print $ generate (sign' (static' :: BelongTo)) (rule' (static' :: BelongTo)) x
    let fil
            | tag == "belong_to" = fmap (((-→?) query :: Relation BelongTo) ⇛) ((-→:) x :: [Relation BelongTo])
            | tag == "commented_at" = fmap (((-→?) query :: Relation CommentedAt) ⇛) ((-→:) x :: [Relation CommentedAt])
            | tag == "created_post" = fmap (((-→?) query :: Relation CreatedPost) ⇛) ((-→:) x :: [Relation CreatedPost])
            | tag == "follow" = fmap (((-→?) query :: Relation Follow) ⇛) ((-→:) x :: [Relation Follow])
            | tag == "lived_in" = fmap (((-→?) query :: Relation LivedIn) ⇛) ((-→:) x :: [Relation LivedIn])
            | tag == "serve" = fmap (((-→?) query :: Relation Serve) ⇛) ((-→:) x :: [Relation Serve])
            | otherwise = []
    print fil
    let res
            | tag == "belong_to" = listMask fil (generate (sign' (static' :: BelongTo)) (rule' (static' :: BelongTo)) x)
            | tag == "commented_at" = listMask fil (generate (sign' (static' :: CommentedAt)) (rule' (static' :: CommentedAt)) x)
            | tag == "created_post" = listMask fil (generate (sign' (static' :: CreatedPost)) (rule' (static' :: CreatedPost)) x)
            | tag == "follow" = listMask fil (generate (sign' (static' :: Follow)) (rule' (static' :: Follow)) x)
            | tag == "lived_in" = listMask fil (generate (sign' (static' :: LivedIn)) (rule' (static' :: LivedIn)) x)
            | tag == "serve" = listMask fil (generate (sign' (static' :: Serve)) (rule' (static' :: Serve)) x)
            | otherwise = []
    print res
    let vidsrc = generateSrcVid x
    let viddst = generateDstVid x
    print vidsrc
    print viddst
    return $ zip3 vidsrc viddst res

gatewayVidQuery :: String -> [String] -> IO [String]
gatewayVidQuery cookie queryvids = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest "http://127.0.0.1:8080/api/db/exec"
    foldr (GHC.Base.liftA2 (:)) (pure [] :: IO [String]) (fmap (func manager initialRequest) queryvids)
    where 
        func manager initialRequest queryvid = do 
            let len = length queryvid
            let queryvid' = "\\\"" ++ drop 2 (take (len - 1) queryvid) ++ "\\\""
            print $ "{\"gql\":\"use demo_sns;match (v) where id(v) == " <> queryvid' <> " return tags(v)\"}"
            let request' = initialRequest {method="POST",requestHeaders =[(hCookie,I.packChars cookie)],requestBody = RequestBodyLBS "{\"gql\":\"use demo_sns;match (v) where id(v) == " <> (RequestBodyLBS . I'.packChars) queryvid' <> " return tags(v);\"}"}  
            response' <- httpLbs request' manager
            putStrLn $ (I'.unpackChars . responseBody) response'
            let tag' = ((I'.unpackChars . responseBody) response' :: String) =~ ("\\\\\"(.*?)\\\\\"" :: String) :: String
            print tag'
            let len' = length tag' 
            let tag = drop 2 (take (len' - 2) tag')
            print tag
            let request = initialRequest {method = "POST",requestHeaders = [(hCookie,I.packChars  cookie)],requestBody = RequestBodyLBS "{\"gql\":\"use  demo_sns; match (v) where id(v) == " <> (RequestBodyLBS . I'.packChars) queryvid' <> " return v;\"}"}
            response <- httpLbs request manager
            let x = (I'.unpackChars . responseBody) response
            let res
                    | tag == "team" =  generate (sign (static :: Team)) (rule (static :: Team)) x
                    | tag == "place" =  generate (sign (static :: Place)) (rule (static :: Place)) x
                    | tag == "player" = generate (sign (static :: Player)) (rule (static :: Player)) x
                    | tag == "post" = generate (sign (static :: Post)) (rule (static :: Post)) x
                    | tag == "address" = generate (sign (static :: Address)) (rule (static :: Address)) x
                    | otherwise = []
            case res of 
                [] -> return ""
                _ -> return $ head res


gatewayQuery :: String -> [String] -> String -> IO [(String,String)]
gatewayQuery cookie query tag = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest "http://127.0.0.1:8080/api/db/exec"
    let request = initialRequest {method = "POST",requestHeaders = [(hCookie,I.packChars  cookie)],requestBody = RequestBodyLBS "{\"gql\":\"use  demo_sns; match (v:" <> (RequestBodyLBS . I'.packChars) tag <> ") return v limit 100\"}"}
    response <- httpLbs request manager
    let x = (I'.unpackChars . responseBody) response
    putStrLn $ x <> "\n\n"
    let fil
          | tag == "team" = fmap (((→?) query :: Node Team) ⇛)  ((→:) x :: [Node Team])
          | tag == "place" = fmap (((→?) query :: Node Place) ⇛) ((→:) x :: [Node Place])
          | tag == "player" = fmap (((→?) query :: Node Player) ⇛) ((→:) x :: [Node Player])
          | tag == "post" = fmap (((→?) query :: Node Post) ⇛) ((→:) x :: [Node Post])
          | tag == "address" = fmap (((→?) query :: Node Address) ⇛) ((→:) x :: [Node Address])
          | otherwise = []
    print fil
    let res
          | tag == "team" = listMask fil (generate (sign (static :: Team)) (rule (static :: Team)) x)
          | tag == "place" = listMask fil (generate (sign (static :: Place)) (rule (static :: Place)) x)
          | tag == "player" = listMask fil (generate (sign (static :: Player)) (rule (static :: Player)) x)
          | tag == "post" = listMask fil (generate (sign (static :: Post)) (rule (static :: Post)) x)
          | tag == "address" = listMask fil (generate (sign (static :: Address)) (rule (static :: Address)) x)
          | otherwise = []
    let vid = generateVid x
    --print vid
    return $ zip vid res

listMask :: [Bool] -> [a] -> [a]
listMask (b:bs) (x:xs) = if b then x:listMask bs xs else listMask bs xs
listMask _ _ = []