{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, StandaloneDeriving #-}
module Oxymoron.Description.Material where
import Data.Singletons
import Oxymoron.Description.Attribute
import Oxymoron.Description.Uniform
import Oxymoron.Description.Varying
import Oxymoron.Description.Program

--The material functions as a cache of the uniform values for the program
--It doesn't add anything in terms of type info (for now), but constitency I've
--added it.
data Material :: [Attribute] -> [Uniform] -> [Varying] -> * where
  Material :: Program a b c -> Material a b c

