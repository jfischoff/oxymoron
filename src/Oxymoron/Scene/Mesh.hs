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
import Oxymoron.Description.Attribute
import qualified Oxymoron.Scene.Attribute as A
import Oxymoron.Scene.Attribute (AttributeState(..))
import Oxymoron.Description.Mesh (IndexType(..))
import Oxymoron.Scene.TypeFunctions
import Data.Singletons.Extras

type instance ToUnpacked 'IUnsignedByte  = Word8
type instance ToUnpacked 'IUnsignedShort = Word16
type instance ToUnpacked 'IUnsignedInt   = Word32

type instance ToUnpacked ('Desc.IndexArray x y) = ToUnpacked x

type IndexArray  (a :: Desc.IndexArray ) = MetaVector a

type instance ToUnpacked ('Desc.VertexArray xs) = ToUnpacked xs

type VertexArray (a :: Desc.VertexArray) = MetaVector a
   
data Mesh :: Desc.Mesh -> * where
     Mesh :: IndexArray  (Desc.GetIndexArray  a)
          -> VertexArray (Desc.GetVertexArray a) 
          -> Mesh a  

data ExMesh :: Desc.VertexArray -> * where
    ExMesh :: Mesh ('Desc.Mesh ia va) -> ExMesh va 
    
{-
bindAttributes :: VertexArray as 'Bound pr -> pr ()
bindAttributes (BoundVertexArray as vts) = mapM (bindAttribute vts) as

renderMesh :: ExMesh as 'Bound pr -> pr ()
renderMesh (ExMesh (MeshV idx vs)) = do
    --break open the vertex array and bind the attributes
    bindAttributes vs
    --draw the elements
    drawElements idx
-}











