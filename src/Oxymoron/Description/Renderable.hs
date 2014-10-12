{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, 
    UndecidableInstances,FlexibleInstances, ScopedTypeVariables, 
    MultiParamTypeClasses, OverlappingInstances, StandaloneDeriving #-}
module Oxymoron.Description.Renderable where
import Data.Singletons
import Data.Singletons.Extras.Set
import Oxymoron.Description.Material
import Oxymoron.Description.Mesh
import Oxymoron.Description.Attribute
import Oxymoron.Description.Varying (Varying)
import Oxymoron.Description.Uniform (Uniform)
import Oxymoron.Description.Symbol (AChar, Symbol(..))

data Renderable :: (Set Attribute) 
                -> (Set Uniform)
                -> (Set Uniform)
                -> (Set Varying) 
                -> * where
    Renderable :: 
        ((mesh_attrs :==: prog_attrs) ~ 'True) 
        => Sing ('[ExMesh ('VertexArray mesh_attrs)]) 
        -> Material   prog_attrs a b c 
        -> Renderable mesh_attrs a b c
          

