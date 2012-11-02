{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Description.Varying where
import Oxymoron.Description.Symbol hiding (insertionSort, insert, Insert, InsertionSort, 
    sInsert, sInsertionSort)
import Data.Singletons
import Prelude hiding (and)

singletons [d|
    data VectorCount = V2 | V3 | V4
                deriving(Show, Eq)
    data VaryingBaseType = Float | SignedInt | UnsignedInt | FloatVector VectorCount
                        | SignedIntVector VectorCount | UnsignedIntVector VectorCount
                deriving(Show, Eq)
    
    data VaryingType = B VaryingBaseType
                     | Array Nat VaryingBaseType
        deriving(Show, Eq)
    
    data Varying = Varying Symbol VaryingType
        deriving(Show, Eq)     
    
    leqVectorCount :: VectorCount -> VectorCount -> Bool
    leqVectorCount V2 V2 = True
    leqVectorCount V2 V3 = False
    leqVectorCount V2 V4 = False
    leqVectorCount V3 V2 = True
    leqVectorCount V3 V3 = True
    leqVectorCount V3 V4 = False
    leqVectorCount V4 V2 = True
    leqVectorCount V4 V3 = True
    leqVectorCount V4 V4 = True
    
    leqVaryingBaseType :: VaryingBaseType -> VaryingBaseType -> Bool
    leqVaryingBaseType Float                 Float                 = True
    leqVaryingBaseType Float                 SignedInt             = False
    leqVaryingBaseType Float                 UnsignedInt           = False
    leqVaryingBaseType Float                 (FloatVector _)       = False
    leqVaryingBaseType Float                 (SignedIntVector _)   = False
    leqVaryingBaseType Float                 (UnsignedIntVector _) = False
    leqVaryingBaseType SignedInt             Float                 = True
    leqVaryingBaseType SignedInt             SignedInt             = True
    leqVaryingBaseType SignedInt             UnsignedInt           = False
    leqVaryingBaseType SignedInt             (FloatVector _)       = False
    leqVaryingBaseType SignedInt             (SignedIntVector _)   = False
    leqVaryingBaseType SignedInt             (UnsignedIntVector _) = False
    leqVaryingBaseType UnsignedInt           Float                 = True
    leqVaryingBaseType UnsignedInt           SignedInt             = True
    leqVaryingBaseType UnsignedInt           UnsignedInt           = True
    leqVaryingBaseType UnsignedInt           (FloatVector _)       = False
    leqVaryingBaseType UnsignedInt           (SignedIntVector _)   = False
    leqVaryingBaseType UnsignedInt           (UnsignedIntVector _) = False
    leqVaryingBaseType (FloatVector _)       Float                 = True
    leqVaryingBaseType (FloatVector _)       SignedInt             = True
    leqVaryingBaseType (FloatVector _)       UnsignedInt           = True
    leqVaryingBaseType (FloatVector x)       (FloatVector y)       = leqVectorCount x y
    leqVaryingBaseType (FloatVector _)       (SignedIntVector _)   = False
    leqVaryingBaseType (FloatVector _)       (UnsignedIntVector _) = False
    leqVaryingBaseType (SignedIntVector _)   Float                 = True
    leqVaryingBaseType (SignedIntVector _)   SignedInt             = True
    leqVaryingBaseType (SignedIntVector _)   UnsignedInt           = True
    leqVaryingBaseType (SignedIntVector _)   (FloatVector _)       = True
    leqVaryingBaseType (SignedIntVector x)   (SignedIntVector y)   = leqVectorCount x y
    leqVaryingBaseType (SignedIntVector _)   (UnsignedIntVector _) = False
    leqVaryingBaseType (UnsignedIntVector _) Float                 = True
    leqVaryingBaseType (UnsignedIntVector _) SignedInt             = True
    leqVaryingBaseType (UnsignedIntVector _) UnsignedInt           = True
    leqVaryingBaseType (UnsignedIntVector _) (FloatVector _)       = True
    leqVaryingBaseType (UnsignedIntVector _) (SignedIntVector _)   = True
    leqVaryingBaseType (UnsignedIntVector x) (UnsignedIntVector y) = leqVectorCount x y
    
    leqVaryingType :: VaryingType -> VaryingType -> Bool
    leqVaryingType (B x) (B y)       = leqVaryingBaseType x y
    leqVaryingType (B _) (Array _ _) = False
    leqVaryingType (Array nx vx) (Array ny vy) = 
            proj (leqNat nx ny) (leqVaryingBaseType vx vy)
    
    leqVarying :: Varying -> Varying -> Bool
    leqVarying (Varying sx vx) (Varying sy vy) = 
            proj (leqSymbol sx sy) (leqVaryingType vx vy) 
    
    
    insert :: Varying -> [Varying] -> [Varying]
    insert n [] = [n]
    insert n (h:t) = if leqVarying n h then (n:h:t) else h:(insert n t)

    insertionSort :: [Varying] -> [Varying]
    insertionSort [] = []
    insertionSort (h:t) = insert h (insertionSort t)
    
    |]
    
type VFloat = 'B 'Float
type VInt = 'B 'SignedInt




testA = [Varying position $ B Float, Varying color $ B Float]
testB = [Varying color $ B Float, Varying position $ B Float]








