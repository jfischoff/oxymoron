{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Description.VertexShader where
import Data.Singletons
import Data.Singletons.Extras.Set
import Oxymoron.Description.Attribute
import Oxymoron.Description.Uniform
import Oxymoron.Description.Varying
import Oxymoron.Description.Symbol

singletons [d| 
   data VertexShader = VertexShader (Set Attribute) (Set Uniform) (Set Varying) 
                    deriving(Show, Eq)|] 
    




