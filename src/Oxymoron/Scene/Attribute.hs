{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Scene.Attribute where
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

data Attribute :: BoundType -> Maybe (Desc.Attribute) -> * where
        NullBound :: GLint      -> Attribute Bound   Nothing
        DescBound :: GLint -> a -> Attribute Bound   (Just a)
        Unbound   :: a          -> Attribute Unbound (Just a)
        
