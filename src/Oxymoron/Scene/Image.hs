{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures, EmptyDataDecls #-}
module Oxymoron.Scene.Image where
import Oxymoron.Scene.Core
import Oxymoron.Scene.Classes
import Data.Array.Repa hiding (Any)
import Graphics.Rendering.OpenGL.Raw
import GHC.TypeLits (Symbol)
import qualified GHC.TypeLits as Lits
import Data.Word
import Data.Int
import Data.Singletons
import Data.Word.Word24

data Image a = Image a (Array r DIM2 (ToRepa a))

type ImageResource a b = Resource a (Image b)

{-
bindImage :: OpenGL m => ImageResource Unbound -> m (ImageResource Bound)
bindImage = undefined
-}

type family ToRepa (a :: PixelFormat) (b :: PixelComponentType) 
type instance ToRepa RED UNSIGNED_BYTE       = Word8
type instance ToRepa RGB UNSIGNED_BYTE_3_3_2 = Word8 -- slowish
type instance ToRepa RGB UNSIGNED_BYTE       = Word24 -- slowish