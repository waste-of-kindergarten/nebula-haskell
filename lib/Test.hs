import Data.Typeable 

data Box1 a = Box1 a deriving(Typeable)

data Box2 b = Box2 b deriving(Typeable)

f :: (Typeable a,Eq a) => Box1 a -> Box2 a -> Bool 
f (Box1 x) y = case gcast y of
            Just (Box2 t) -> t == x 
            Nothing -> False   

main :: IO ()
main = 
    do 
        manager <- newManager defaultManagerSettings
        initialReuqest <- parseRequest "http://12.0.0.1:8080/api/db/exec"
        let request = initialRequest {method = "POST",requestBody = RequestBodyLBS "{\"gql\":\"use  demo_football_2022;match  p=(v1 : player)--(v2) return v2 limit 2\"} "}