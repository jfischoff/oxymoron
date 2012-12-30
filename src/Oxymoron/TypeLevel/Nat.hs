{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, TemplateHaskell #-}
module Oxymoron.TypeLevel.Nat where
import Data.Singletons

singletons [d|
   -- Basic Nat type
   data Nat = Zero | Succ Nat deriving (Eq, Ord)
   
   leqNat :: Nat -> Nat -> Bool
   leqNat Zero _ = True
   leqNat (Succ _) Zero = False
   leqNat (Succ a) (Succ b) = leqNat a b
   |]

-- Conversions to any from Integers
fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Succ n) = (fromNat n) + 1

toNat :: Integer -> Nat
toNat 0         = Zero
toNat n | n > 0 = Succ (toNat (n - 1))
toNat _         = error "Converting negative to Nat"