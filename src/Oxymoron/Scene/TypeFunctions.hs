{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    KindSignatures,
    OverlappingInstances #-}
module Oxymoron.Scene.TypeFunctions where
import Data.Singletons    
import GHC.Generics

type family Map (f :: a -> b) (xs :: [a]) :: [b]
type instance Map f '[] = '[]
type instance Map f  (x ': xs) = f x ': Map f xs

type family (f :: a -> b) :$ (x :: a) :: b
type instance f :$ x = f x

type family ToProd (xs :: [a]) :: *
type instance ToProd (x ': y ': xs) = 
    If (xs :==: '[])
        (x, y)
        (x, (y, ToProd xs))


     