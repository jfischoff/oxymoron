{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Description.Uniform where
import Oxymoron.Description.Symbol
import Data.Singletons

singletons [d| 
    -- TODO actually fill this out
    data UniformType = UTFloat
        deriving(Show, Eq)
    data Uniform = Uniform Symbol UniformType
        deriving(Show, Eq)
    |]
