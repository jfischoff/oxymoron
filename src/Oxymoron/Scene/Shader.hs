{-# LANGUAGE GADTs, DataKinds, KindSignatures, RankNTypes, MultiParamTypeClasses,
    TypeSynonymInstances, FlexibleInstances, ExistentialQuantification #-}
module Oxymoron.Scene.Shader where
import Language.GLSL.Syntax
import Graphics.Rendering.OpenGL.Raw
import Oxymoron.Scene.Classes
import Oxymoron.Scene.Core

--ShaderDesc :: 
singletons [d| data ShaderType = Vertex Desc.VertexShader | Fragment Desc.FragmentShader deriving (Show, Eq, Ord) |]

data ShaderFlavour = Parsed | Sourced  
data ShaderState   = ShaderCreated | Moved | Compiled | ShaderDestroyed

-- Ideally this would be different and I would
-- have an AST for the GLSL code that was typed in
-- the same way as the description, but for now
-- I am faking it.
-- either String or TranslationUnit    
data ShaderValue :: ShaderType -> ShaderFlavour -> * where 
    ShaderValue :: a (If (b ~ 'Parsed) TranslationUnit String) :: ShaderValue a b
    
--I probably want to make a seperate vertex shader so I can convert to a 
--so I can have an attribute type that can be bound
-

type ShaderResource a b c = Resource a (ShaderValue b c)     

data Shader :: ShaderType -> ShaderFlavour -> ShaderState -> * where 
    ShaderCreate  :: ShaderResource Allocated a b   -> Shader a b ShaderCreated  
    ShaderMove    :: Shader a b ShaderCreated       -> Shader a b Moved          
    ShaderCompile :: Shader a b Moved               -> Shader a b Compiled       
    ShaderDestroy :: ShaderResource Unallocated a b -> Shader a b ShaderDestroyed
    
type VertexShader   a b c = Shader a (Vertex b)   c
type FragmentShader a b c = Shader a (Fragment b) c

{-    
data AnyShaderState a b = forall c. AnyShaderState (Shader a b c)
    
instance (Monad m) => GLTransition m (AnyShaderState a b) where
    glStep (AnyShaderState r) = case r of
        ShaderCreate y -> return . AnyShaderState . ShaderMove    . ShaderCreate $ y
        ShaderMove   y -> return . AnyShaderState . ShaderCompile . ShaderMove   $ y
        _              -> return . AnyShaderState $ r
-}


{-
    
createVertexShader :: OpenGL m => Shader a Sourced -> m (VertexShader Created)
createVertexShader = undefined

createFragmentShader :: OpenGL m => Shader a Sourced -> m (FragmentShader Created)
createFragmentShader = undefined

compileShader :: OpenGL m => Shader a Created -> m (Shader a Compiled)
compileShader x = undefined
    
prettyPrintShader :: Shader a Parsed -> Shader a Sourced
prettyPrintShader = undefined

parseShader :: Shader a Sourced -> Shader a Parsed
parseShader = undefined



-}
    
