{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Scene.Uniform where
import Oxymoron.Scene.Image
import Oxymoron.Scene.Core
import Oxymoron.Scene.Classes
import Data.Word
import Graphics.Rendering.OpenGL.Raw
import GHC.TypeLits (Symbol)
import qualified GHC.TypeLits as Lits
import Data.Word
import Data.Int
import Data.Singletons


data UniformType a = UniformType a (ToUniformValue a)
type Uniform a b = Reference a (UniformValue b)

--instance IsAllocated UTUnsignedInt
--instance IsAllocated (UTImage Allocated b)

--glUniform :: (OpenGL m, IsAllocated a) => Uniform Bound a -> m ()
--glUniform = undefined

--testGood :: (OpenGL m) => m ()
--testGood = glUniform (undefined :: Uniform Bound (UTImage Allocated TEXTURE_2D))

--testBad :: (OpenGL m) => m ()
--testBad = glUniform (undefined :: Uniform Bound (UTImage Unallocated TEXTURE_2D))



















