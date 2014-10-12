{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Scene.Material where
import Oxymoron.Scene.Program
import Oxymoron.Scene.Uniform
import Data.Singletons
import Data.Singletons.Extras.Set as S
import qualified Oxymoron.Description.Attribute as Desc
import qualified Oxymoron.Description.Uniform as Desc
import qualified Oxymoron.Description.Varying as Desc


data Material :: *
              -> ProgramState
              -> * where
     Material :: Program a b 
--              -> Sing (S.Map (Uniform r) (S.Union b c))
              -> Material a b
              





              
 

