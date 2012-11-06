{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Scene.Uniform where
import Oxymoron.Scene.Resource
import Data.Singletons
import qualified Oxymoron.Description.Uniform as Desc
import  Oxymoron.Description.Uniform (UniformType(..), Uniform(..))

newtype Uniform r a = U (R (UniformValue a) r)

data family UniformValue (a :: Desc.Uniform)
data instance UniformValue a = VUniform (Sing a) (UniformValue a)
    
-- these guys should probably be GADTs    
data family UniformValueType (a :: Desc.Uniform)
data instance UniformValueType ('Uniform x 'UTFloat) = UVFloat Float 

--data UniformType a = UniformType a (ToUniformValue a)
--type Uniform a b = Reference a (UniformValue b)

--instance IsAllocated UTUnsignedInt
--instance IsAllocated (UTImage Allocated b)

--glUniform :: (OpenGL m, IsAllocated a) => Uniform Bound a -> m ()
--glUniform = undefined

--testGood :: (OpenGL m) => m ()
--testGood = glUniform (undefined :: Uniform Bound (UTImage Allocated TEXTURE_2D))

--testBad :: (OpenGL m) => m ()
--testBad = glUniform (undefined :: Uniform Bound (UTImage Unallocated TEXTURE_2D))



















