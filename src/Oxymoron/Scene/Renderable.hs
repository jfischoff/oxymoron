{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Scene.Renderable where
--import Oxymoron.Scene.Program
import Oxymoron.Scene.Mesh
import Data.Singletons
import Oxymoron.Scene.Material
import qualified Oxymoron.Description.Attribute as Desc
import Oxymoron.Description.Attribute (InsertionSort)
import qualified Oxymoron.Description.Uniform as Desc
import qualified Oxymoron.Description.Varying as Desc
import Oxymoron.Scene.Program
import Oxymoron.Scene.Attribute

--This is where I need to make sure that I have a resource for the 
--the attributes 

singletons [d| 
    data RenderableState = CreatedRenderable ProgramState AttributeState
                                    | BoundRenderable ProgramState
            |]

data Renderable :: [Desc.Attribute] 
                -> [Desc.Uniform] 
                -> [Desc.Uniform] 
                -> [Desc.Varying]
                -> RenderableState
                -> (* -> *) 
                -> * where
    CreateRenderable :: Material as b c d e r 
               -> [ExMesh as aState r]
               -> Renderable as b c d ('CreatedRenderable e aState) r
    BindRenderable :: (((InsertionSort xs) :==: (InsertionSort as)) ~ 'True)
                    => Material as b c d e r 
                    -> [ExMesh xs 'Bound r]
                    -> Renderable as b c d ('BoundRenderable e) r


data ExRenderable :: RenderableState -> (* -> *) -> * where
    ExRenderable :: forall a b c d e f. Renderable a b c d e f
                 -> ExRenderable e f
                 
--TODO this should also work validated
render :: Renderable a b c d ('BoundRenderable 'Linked) pr -> pr ()
render (BindRenderable mat meshes) = do
    --TODO figure out how to make so that the 
    --the all of the gl 
    glUseProgram mat $ mapM_ renderMesh meshes
        
    



























