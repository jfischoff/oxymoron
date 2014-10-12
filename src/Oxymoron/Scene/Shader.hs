{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, TemplateHaskell #-}
module Oxymoron.Scene.Shader where
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Graphics.Rendering.OpenGL.Raw (GLint)
import qualified Oxymoron.Description.Shader as Desc
import Data.Singletons
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Graphics.Rendering.OpenGL.Raw.Core32 as GL
import Foreign.Ptr
import Control.Lens

--I need to refactor this so that it doesn't use states
--so what is a shader?
--it is the a description, flavor, component, and resource
--I am pretty sure. 

singletons [d| 
    data ShaderState   = Created | Moved | Compiled 
    data ShaderFlavour = AST | Sourced 
    data ShaderComponent = Vertex | Fragment 
    
    |]
    

singletons [d| 
    data ShaderType = ShaderType Desc.Shader ShaderFlavour ShaderComponent

    getFlavour :: ShaderType -> ShaderFlavour
    getFlavour (ShaderType _ x _) = x
    
    getDesc :: ShaderType -> Desc.Shader
    getDesc (ShaderType x _ _) = x

    |]

data Sources = Sources {
        _count   :: GL.GLsizei,
        _strings :: Ptr (Ptr GL.GLchar), 
        _lengths :: Ptr GLint
    }

makeLenses ''Sources

data ShaderInfo :: ShaderType -> * where
    ShaderInfo :: Sing a 
               -> Sources
               -> ShaderInfo a


data Shader :: ShaderType
                  -> ShaderState
                  -> * where
    Shader :: ShaderInfo a
                 -> SShaderState s 
                 -> Shader a s
                 
shaderState :: Lens (Shader a b ) 
                    (Shader a z ) 
                    (SShaderState b)
                    (SShaderState z) 
shaderState = error "shaderState"


sources :: Lens (Shader ('ShaderType a 'Sourced e) b ) 
                (Shader ('ShaderType a 'Sourced e) b ) 
                Sources
                Sources
sources = error "sources"     

shaderId = error "shaderId"   

shaderComp = error "shaderComp"
glEnum = error "glEnum"

-- I need to add shaders to the enviroment when ever I make them

{-
glCreateShader :: MonadIO pr 
               => ShaderInfo a 
               -> RegionT s pr (Shader a 'Created (RegionT s pr))
glCreateShader x = do
    shaderId <- liftIO $ GL.glCreateShader $ x^.shaderComp^.glEnum
    fin      <- onExit $ GL.glDeleteShader shaderId
    return $ Shader (R x shaderId fin) SCreated            

glShaderSource :: (a ~ ('ShaderType z 'Sourced e), 
                  AncestorRegion pr cr,
                  MonadIO cr) 
               => Shader a b pr 
               -> cr (Shader a Moved pr)
glShaderSource x = do 
    let shaderId'      = x^.shaderId
        sourcesCount   = x^.sources.count
        sourcesStrings = x^.sources.strings
        sourcesLengths = x^.sources.lengths
        
    liftIO $ GL.glShaderSource shaderId' sourcesCount sourcesStrings sourcesLengths
    return $ set shaderState SMoved x
-}








    
