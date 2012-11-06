{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Scene.Mesh where
import Data.Singletons
import Data.Array.Repa hiding (Any)
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import Data.Int
import Graphics.Rendering.OpenGL.Raw
import qualified Oxymoron.Description.Mesh as Desc
import Oxymoron.Description.Mesh (Mesh(..))
import Oxymoron.Description.Attribute
import qualified Oxymoron.Scene.Attribute as A
import Oxymoron.Scene.Attribute (AttributeState(..))
import Oxymoron.Description.Mesh (IndexType(..))
import Oxymoron.Scene.TypeFunctions
import Foreign.Storable.Tuple

data IndexArray :: Desc.IndexType -> * where
    IndexArray :: Sing a 
               -> Array F DIM1 (ToIdxValue a) 
               -> IndexArray a
               
data VertexArray :: [Attribute] 
                 -> A.AttributeState 
                 -> (* -> *) 
                 -> * where
    CreateVertexArray :: SList (Map (A.Attribute 'Created r) as) 
                      -> Array F DIM1 (ToProd (Map ToTuple as))
                      -> VertexArray as 'Created r
    BoundVertexArray :: SList (Map (A.Attribute 'Bound r) as)
                     -> Array F DIM1 (ToProd (Map ToTuple as))
                     -> VertexArray as 'Bound r
                       
data MeshV :: Desc.Mesh -> AttributeState -> (* -> *) ->  * where
     MeshV :: IndexArray  (Desc.GetIndexType a) 
           -> VertexArray (Desc.GetAttributes a) b r
           -> MeshV a b r

data ExMesh :: [Attribute] -> AttributeState -> (* -> *) -> * where
    ExMesh :: forall i s as r. MeshV ('Mesh i as) s r -> ExMesh as s r
    

     
type family ToIdxValue (a :: IndexType)
type instance ToIdxValue 'IUnsignedByte  = Word8
type instance ToIdxValue 'IUnsignedShort = Word16
type instance ToIdxValue 'IUnsignedInt   = Word32 

data family ToTuple (x :: Attribute)
data instance ToTuple ('Attribute x a 'C2) = Pair
        (ToVertexValue a) (ToVertexValue a)
data instance ToTuple ('Attribute x a 'C3) = Triple
        (ToVertexValue a) (ToVertexValue a) (ToVertexValue a)
data instance ToTuple ('Attribute x a 'C4) = Quad
        (ToVertexValue a) (ToVertexValue a) (ToVertexValue a) (ToVertexValue a)

type family ToVertexValue (x :: ValueType) 
type instance ToVertexValue VTByte  = Word8
type instance ToVertexValue VTShort = Int16
type instance ToVertexValue VTFloat = Float

bindAttributes :: VertexArray as 'Bound pr -> pr ()
bindAttributes (BoundVertexArray as vts) = mapM (bindAttribute vts) as

bindAttribute :: 


renderMesh :: ExMesh as 'Bound pr -> pr ()
renderMesh (ExMesh (MeshV idx vs)) = do
    --break open the vertex array and bind the attributes
    bindAttributes vs
    --draw the elements
    drawElements idx














