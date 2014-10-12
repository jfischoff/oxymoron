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
import qualified Oxymoron.Description.Program as Desc
import qualified Oxymoron.Description.Varying as Desc
import qualified Oxymoron.Description.Mesh as Desc
import Oxymoron.Scene.Program
import Oxymoron.Scene.Attribute

--This is where I need to make sure that I have a resource for the 
--the attributes 

-- okay I need a way to work with the different states
-- and the resources

-- The simplest way is to add a Maybe field
-- no state less
-- just see if you can render a renderable where the
-- there is no run loop
-- only an initialization


singletons [d| 
    data RenderableState = CreatedRenderable ProgramState AttributeState
                                    | BoundRenderable ProgramState
            |]

data Renderable :: * -> * where
   Renderable :: Material (Desc.Program as b c d) e  
              -> [ExMesh ('Desc.VertexArray as)]
              -> Renderable (Desc.Program as b c d) 



--data ExRenderable :: RenderableState -> (* -> *) -> * where
--    ExRenderable :: forall a b c d e f. Renderable a b c d e f
--                 -> ExRenderable e f
                 
render :: Renderable a b -> IO ()
render (Renderable mat meshes) = do
  
  
  
    
--    glUseProgram mat $ mapM_ renderMesh meshes
    
--glUseProgram :: Material 
        
    



























