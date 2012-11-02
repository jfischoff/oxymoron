{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Description.Program where
import Data.Singletons
import Oxymoron.Description.VertexShader
import Oxymoron.Description.FragmentShader
import Oxymoron.Description.Varying 
import Oxymoron.Description.Attribute (Attribute(..), Color(..))
import Oxymoron.Description.Uniform
import Oxymoron.Description.Symbol (AChar(..), Symbol(..))


-- The outputs must match the inputs 
data Program :: [Attribute] -> [Uniform] -> [Varying] -> * where
  Program :: (((InsertionSort v_output) :==: (InsertionSort s_input)) ~ 'True) 
          => Sing ('VertexShader a xs v_output) 
          -> Sing ('FragmentShader s_input ys) 
          -> Program a (xs :++ ys) v_output  
          




    
    