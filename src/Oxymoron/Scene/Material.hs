{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Scene.Material where
import Oxymoron.Scene.Program
import Oxymoron.Scene.Uniform
import Data.Singletons
import qualified Oxymoron.Description.Attribute as Desc
import qualified Oxymoron.Description.Uniform as Desc
import qualified Oxymoron.Description.Varying as Desc


data Material :: [Desc.Attribute] 
              -> [Desc.Uniform] 
              -> [Desc.Uniform] 
              -> [Desc.Varying]
              -> ProgramState
              -> (* -> *)
              -> * where
     Material :: Program a b c d e r  
              -> SList (Map (Uniform r) (b :++ c)) 
              -> Material a b c d e r
              
type family Map (f :: a -> b) (xs :: [a]) :: [b]
type instance Map f '[] = '[]
type instance Map f  (x ': xs) = f x ': Map f xs





              
 

