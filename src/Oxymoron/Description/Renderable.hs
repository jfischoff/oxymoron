{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, 
    UndecidableInstances,FlexibleInstances, ScopedTypeVariables, 
    MultiParamTypeClasses, OverlappingInstances, StandaloneDeriving #-}
module Oxymoron.Description.Renderable where
import Data.Singletons
import Oxymoron.Description.Material
import Oxymoron.Description.Mesh
import Oxymoron.Description.Attribute
import Oxymoron.Description.Varying (Varying)
import Oxymoron.Description.Uniform (Uniform)
import Oxymoron.Description.Symbol (AChar, Symbol(..))

data Renderable :: [Attribute] -> [Uniform] -> [Uniform] -> [Varying] -> * where
    Renderable :: 
        (((InsertionSort mesh_attrs) :==: (InsertionSort prog_attrs)) ~ 'True) 
        => Sing ('[ExMesh mesh_attrs]) 
        -> Material prog_attrs a b c 
        -> Renderable mesh_attrs a b c
            

