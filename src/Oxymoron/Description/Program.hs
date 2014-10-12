{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies, 
    EmptyDataDecls, GADTs, TypeOperators, RankNTypes, FlexibleContexts, 
    UndecidableInstances,FlexibleInstances, ScopedTypeVariables, 
    MultiParamTypeClasses, OverlappingInstances #-}
module Oxymoron.Description.Program where
import Data.Singletons
import Oxymoron.Description.VertexShader
import Oxymoron.Description.FragmentShader
import Oxymoron.Description.Varying 
import Oxymoron.Description.Attribute (Attribute(..))
import Oxymoron.Description.Uniform
import Oxymoron.Description.Symbol (AChar(..), Symbol(..))
import Data.Singletons.Extras.Set

-- I think I can make a type level set

-- The outputs must match the inputs 
data Program :: Set Attribute -> Set Uniform -> Set Uniform -> Set Varying -> * where
  Program :: (( output :==: input ) ~ 'True) 
          => Sing ('VertexShader a xs output) 
          -> Sing ('FragmentShader input ys)
          -> Program a xs ys output  
          
          -- TODO get rid of the duplicate Uniforms
          







    
    