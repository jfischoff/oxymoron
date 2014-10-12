{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Scene.Uniform where
import Data.Singletons
import qualified Oxymoron.Description.Uniform as Desc

data Uniform :: Desc.Uniform -> * where
    Uniform :: Sing a -> ToUniformValue a -> Uniform a

type family ToUniformValue (a :: Desc.Uniform) :: *

--instance IsAllocated UTUnsignedInt
--instance IsAllocated (UTImage Allocated b)

--glUniform :: (OpenGL m, IsAllocated a) => Uniform Bound a -> m ()
--glUniform = undefined

--testGood :: (OpenGL m) => m ()
--testGood = glUniform (undefined :: Uniform Bound (UTImage Allocated TEXTURE_2D))

--testBad :: (OpenGL m) => m ()
--testBad = glUniform (undefined :: Uniform Bound (UTImage Unallocated TEXTURE_2D))



















