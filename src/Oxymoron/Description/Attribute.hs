-- | A Attribute describes a named array of data types
{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Description.Attribute where
import Oxymoron.Description.Symbol hiding (insertionSort, insert, 
    Insert, InsertionSort, sInsert, sInsertionSort)
import Data.Singletons
import Prelude hiding (and)

singletons [d|
    -- some predefined types
    -- TODO remove when I have a Quasiquoter for the Symbol type
    
    data CoordinateType = C2 | C3 | C4
        deriving(Show, Eq)
    data ValueType      = VTByte | VTShort | VTFloat
        deriving(Show, Eq)
    data Attribute = Attribute Symbol ValueType CoordinateType
        deriving(Show, Eq)
    
    leqCoordinateType :: CoordinateType -> CoordinateType -> Bool
    leqCoordinateType C2 C2 = True
    leqCoordinateType C2 C3 = False
    leqCoordinateType C2 C4 = False
    leqCoordinateType C3 C2 = True
    leqCoordinateType C3 C3 = True
    leqCoordinateType C3 C4 = False
    leqCoordinateType C4 C2 = True
    leqCoordinateType C4 C3 = True
    leqCoordinateType C4 C4 = True
    
    leqValueType :: ValueType -> ValueType -> Bool
    leqValueType VTByte  VTByte  = True
    leqValueType VTByte  VTShort = False
    leqValueType VTByte  VTFloat = False
    leqValueType VTShort VTByte  = True
    leqValueType VTShort VTShort = True
    leqValueType VTShort VTFloat = False
    leqValueType VTFloat VTByte  = True
    leqValueType VTFloat VTShort = True
    leqValueType VTFloat VTFloat = True

     
    leqAttribute :: Attribute -> Attribute -> Bool
    leqAttribute (Attribute sx vx cx) (Attribute sy vy cy) =   
        proj (proj (leqSymbol sx sy) (leqValueType vx vy)) (leqCoordinateType cx cy)
    
    insert :: Attribute -> [Attribute] -> [Attribute]
    insert n [] = [n]
    insert n (h:t) = if leqAttribute n h then (n:h:t) else h:(insert n t)

    insertionSort :: [Attribute] -> [Attribute]
    insertionSort [] = []
    insertionSort (h:t) = insert h (insertionSort t)
    
    --TODO make this work!
    findAttr :: Symbol -> [Attribute] -> Maybe Attribute
    findAttr n (x:xs) = Nothing
    
    fromJust :: Maybe a -> a
    fromJust (Just a) = a
    
    |]

