{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Description.FragmentShader where
import Data.Singletons
import Data.Singletons.Extras.Set
import Oxymoron.Description.Uniform
import Oxymoron.Description.Varying
import Oxymoron.Description.Symbol

-- TODO and fragment shader outputs (render targets)
singletons [d| 
   data FragmentShader = FragmentShader (Set Varying) (Set Uniform) 
      deriving (Show, Eq) 
   
   |] 
    




