{-# Language PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures, DeriveDataTypeable #-}
module Oxymoron.Description.Symbol where
import Data.Singletons
import Language.Haskell.TH
import Data.Data


singletons [d| 
    data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

    data AChar = CA | CB | CC | CD | CE | CF | CG | CH | CI
               | CJ | CK | CL | CM | CN | CO | CP | CQ | CR
               | CS | CT | CU | CV | CW | CX | CY | CZ
               deriving (Read, Show, Eq) 
    data Symbol = Symbol [AChar]
               deriving (Read, Show, Eq) 
               
    data SymbolList = SymbolList [Symbol]
    
    and :: Bool -> Bool -> Bool
    and True True = True
    and True False = False
    and False True = False
    and False False = False
    
    leqNat :: Nat -> Nat -> Bool
    leqNat Zero _ = True
    leqNat (Succ _) Zero = False
    leqNat (Succ a) (Succ b) = leqNat a b
    
    leqAChar :: AChar -> AChar -> Bool
    leqAChar CA _  = True
    leqAChar CB CA = False
    leqAChar CB _  = False
    
    leqSymbol :: Symbol -> Symbol -> Bool
    leqSymbol (Symbol []) (Symbol [])         = True
    leqSymbol (Symbol (x:_))  (Symbol [])         = False 
    leqSymbol (Symbol (x:xs)) (Symbol (y:ys)) = and (leqAChar x y) 
        (leqSymbol (Symbol xs) (Symbol ys))
    
    --write the one for symbol

    insert :: Symbol -> [Symbol] -> [Symbol]
    insert n [] = [n]
    insert n (h:t) = if leqSymbol n h then (n:h:t) else h:(insert n t)

    insertionSort :: [Symbol] -> [Symbol]
    insertionSort [] = []
    insertionSort (h:t) = insert h (insertionSort t)
    
    -- I think I will need to write this again for 
    
    |]
      
-- Conversions to any from Integers
fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Succ n) = (fromNat n) + 1

toNat :: Integer -> Nat
toNat 0         = Zero
toNat n | n > 0 = Succ (toNat (n - 1))
toNat _         = error "Converting negative to Nat"   
      
      
      
      
      
      
               
