{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
module Oxymoron.Scene.Renderable where
--import Oxymoron.Scene.Program
import Oxymoron.Scene.MeshDesc
import Oxymoron.Scene.VertexDesc

class VertexAndMeshAttributesMatch a where


{-
data RenderableStates = RenderableCreated MaterialState MeshState
                      | RenderableSetup
                      | RenderableToredown
                      
data BoundStreamDesc :: ValueType -> CoordinateType -> * where
      BoundStreamDesc :: GLint -> SValueType a -> SCoordinateType b -> BoundStreamDesc a b
-}                    
