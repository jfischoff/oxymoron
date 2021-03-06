-- | A Attribute describes a named array of data types
{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Description.Attribute where
--import Oxymoron.Description.Symbol hiding (insertionSort, insert, 
--    Insert, InsertionSort, sInsert, sInsertionSort)
import Data.Singletons
import Data.Singletons.Extras hiding ( insertionSort
                                     , InsertionSort
                                     , Insert
                                     , sInsertionSort
                                     , sInsert
                                     )
import Prelude hiding (and)
import Data.Word
import Data.Int

singletons [d|
    
    data CoordinateType = C2 | C3 | C4
        deriving(Show, Eq)
    data ValueType = VTByte | VTShort | VTFloat
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

type instance ToUnpacked ('Attribute x 'VTByte 'C2) = (Word8, Word8)
type instance ToUnpacked ('Attribute x 'VTByte 'C3) = (Word8, Word8, Word8)
type instance ToUnpacked ('Attribute x 'VTByte 'C4) = (Word8, Word8, Word8, Word8)

type instance ToUnpacked ('Attribute x 'VTShort 'C2) = (Int16, Int16)
type instance ToUnpacked ('Attribute x 'VTShort 'C3) = (Int16, Int16, Int16)
type instance ToUnpacked ('Attribute x 'VTShort 'C4) = (Int16, Int16, Int16, Int16)

type instance ToUnpacked ('Attribute x 'VTFloat 'C2) = (Float, Float)
type instance ToUnpacked ('Attribute x 'VTFloat 'C3) = (Float, Float, Float)
type instance ToUnpacked ('Attribute x 'VTFloat 'C4) = (Float, Float, Float, Float)
