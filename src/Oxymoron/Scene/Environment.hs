{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, TemplateHaskell #-}
module Oxymoron.Scene.Environment where
import Oxymoron.Description
import Oxymoron.Regions
import Data.Singletons 
import Graphics.Rendering.OpenGL.Raw
import Oxymoron.TypeLevel.AssociativeArray

-- (Maybe (ImageTarget, (R Image c)))
-- okay
-- what I am starting to think 
-- is that I should be modeling the opengl state
-- and I should include a description of the shader there
-- so...
-- I would have a id that is associated with a shader
-- the shader would have a state that and potentially a description
-- So the program would just have ids to shaders

-- all of the safety stuff goes first
-- then the renderable and material stuff

singletons [d| 
    data ShaderState   = ShaderCreated  | Moved | Compiled
    data ProgramState  = ProgramCreated | VertexAttached   ShaderState
                       | FragmentAttached ShaderState
                       | Attached ShaderState ShaderState
                       | Linked 
                       | Validated
                      |]
                      


-- Maybe Program -> Maybe Image                
data Env :: Maybe * 
         -> Maybe (ImageTarget, *) 
         -> AssocArray Nat ShaderState 
         -> AssocArray Nat ProgramState 
         -> * where 
    Env :: {
            currentProgram :: Sing a,
            currentImage   :: Sing b,
            shaderStates   :: Sing c,
            programStates  :: Sing d
        } -> Env a b c d





