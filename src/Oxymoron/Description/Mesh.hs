{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Description.Mesh where
import Data.Singletons
import Oxymoron.Description.Attribute

singletons [d|
    data IndexType   = IUnsignedByte | IUnsignedShort | IUnsignedInt
        deriving(Show, Eq)
    data PolygonType = TRIANGLES | TRIANGLE_STRIP
        deriving(Show, Eq)
    data IndexArray  = IndexArray IndexType PolygonType
        deriving(Show, Eq)
    data Mesh = Mesh IndexArray [Attribute]
        deriving(Show, Eq)
    |]

-- Hide the index type since it can vary in the renderable and is 
-- not important for some verification
data ExMesh a = forall b. ExMesh (Sing ('Mesh b a))















