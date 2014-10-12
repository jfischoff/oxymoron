{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Description.Mesh where
import Data.Singletons
import Data.Singletons.Extras.Set
import Oxymoron.Description.Attribute

singletons [d|
    data IndexType   = IUnsignedByte | IUnsignedShort | IUnsignedInt
        deriving(Show, Eq)
    data PolygonType = TRIANGLES | TRIANGLE_STRIP
        deriving(Show, Eq)
    data IndexArray  = IndexArray IndexType PolygonType
        deriving(Show, Eq)    
        
    getIndexType :: IndexArray -> IndexType
    getIndexType (IndexArray x _) = x
    
    getPolygonType :: IndexArray -> PolygonType
    getPolygonType (IndexArray _ x) = x
    
    data VertexArray = VertexArray (Set Attribute)
        deriving(Show, Eq)
    
    data Mesh = Mesh IndexArray VertexArray
        deriving(Show, Eq)
    
    getIndexArray :: Mesh -> IndexArray
    getIndexArray (Mesh x _) = x
    
    getVertexArray :: Mesh -> VertexArray
    getVertexArray (Mesh _ xs) = xs
    
    |]


-- Hide the index type since it can vary in the renderable and is 
-- not important for some verification
data ExMesh a = forall b. ExMesh (Sing ('Mesh b a))















