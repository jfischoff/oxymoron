{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Scene.Mesh where
import GHC.TypeLits (Symbol)
import qualified GHC.TypeLits as Lits
import Data.Word
import Data.Int
import Data.Singletons
import Data.Array.Repa hiding (Any)
import Graphics.Rendering.OpenGL.Raw
import Oxymoron.Scene.Core
import Oxymoron.Scene.Classes
import Data.Default


data Mesh (a :: Desc.Mesh) = Mesh a (ToIndexArray a) (ToVertexArray a)










