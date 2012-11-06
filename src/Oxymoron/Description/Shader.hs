{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Description.Shader where
import Data.Singletons
import Oxymoron.Description.Uniform
import Oxymoron.Description.Varying
import Oxymoron.Description.Symbol
import Oxymoron.Description.FragmentShader
import Oxymoron.Description.VertexShader

singletons [d| 
    data Shader = V VertexShader | F FragmentShader 
    
    isVertexShader :: Shader -> Bool
    isVertexShader (V _ ) = True
    isVertexShader (F _ ) = False    
    
    getVertexShader :: Shader -> VertexShader
    getVertexShader (V x) = x
    
    getFragmentShader :: Shader -> FragmentShader
    getFragmentShader (F x) = x
    
    |]