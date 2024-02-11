{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
module NGraph where
import Data.Data (Typeable, cast)

data All

data Nul

data Node a where
    Node :: {element :: a} -> Node a
    QueryNode :: {condition :: a -> Bool} -> Node a
    AnyNode :: Node All
    EmptyNode :: Node Nul
    deriving(Typeable)


data Relation a where
    Relation :: {relation :: a} -> Relation a
    QueryRelation :: {rcondition :: a -> Bool} -> Relation a
    AnyRelation :: Relation All
    EmptyRelation :: Relation Nul
    deriving(Typeable)

(=?) :: (Typeable a, Typeable b, Eq a, Eq b) => a -> b -> Bool
x =? y = case cast x of
    Just x' -> x' == y
    Nothing -> False

class GeneralMatch f where
    (≟) :: (Typeable a,Typeable b,Eq a,Eq b) => f a -> f b -> Bool
    (⇛) :: (Typeable a,Typeable b,Eq a,Eq b) => f a -> f b -> Bool

instance GeneralMatch Node where
    (≟) :: (Typeable a,Typeable b,Eq a,Eq b) => Node a -> Node b -> Bool
    Node a ≟ Node b = a =? b
    _ ≟ _ = False
    (⇛) :: (Typeable a,Typeable b,Eq a,Eq b) => Node a -> Node b -> Bool
    AnyNode ⇛ Node _ = True
    QueryNode f ⇛ Node a = maybe False f (cast a)
    _ ⇛ _ = False

instance GeneralMatch Relation where 
    (≟) :: (Typeable a,Typeable b,Eq a,Eq b) => Relation a -> Relation b -> Bool 
    Relation a ≟ Relation b = a =? b 
    _ ≟ _ = False 
    (⇛) :: (Typeable a,Typeable b,Eq a,Eq b) => Relation a -> Relation b -> Bool 
    AnyRelation ⇛ Relation _ = True 
    QueryRelation f ⇛ Relation a = maybe False f (cast a) 
    _ ⇛ _ = False  

