{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, TemplateHaskell #-}
module Oxymoron.Scene.Shader where
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Graphics.Rendering.OpenGL.Raw (GLint)
import qualified Oxymoron.Description.Shader as Desc
import Data.Singletons
import Language.GLSL.Syntax
import Control.Monad.Trans.Region
import Control.Monad.Trans.Region.OnExit
import Oxymoron.Scene.Resource
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Graphics.Rendering.OpenGL.Raw.Core32 as GL
import Foreign.Ptr
import Control.Lens

--ShaderDesc :: 
---singletons [d| data ShaderType = Vertex Desc.VertexShader | Fragment Desc.FragmentShader deriving (Show, Eq, Ord) |]

singletons [d| 
    data ShaderState   = Created | Moved | Compiled 
    data ShaderFlavour = AST | Sourced 
    data ShaderComponent = Vertex | Fragment |]
    

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
               -> If ((GetFlavour a) :==: 'AST) TranslationUnit Sources
               -> ShaderInfo a


data Shader :: ShaderType
                  -> ShaderState
                  -> (* -> *) 
                  -> * where
    Shader :: R (ShaderInfo a) r
                 -> SShaderState s 
                 -> Shader a s r
                 
shaderState :: Lens (Shader a b c) 
                    (Shader a z c) 
                    (SShaderState b)
                    (SShaderState z) 
shaderState = error "shaderState"


sources :: Lens (Shader ('ShaderType a 'Sourced e) b c) 
                (Shader ('ShaderType a 'Sourced e) b c) 
                Sources
                Sources
sources = error "sources"     

shaderId = error "shaderId"   

shaderComp = error "shaderComp"
glEnum = error "glEnum"

--TODO refactor so type is part of the shader
--Fix this and then make sure the whole pipeline can compile
--or something like that 
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
    

glShaderCompile :: (AncestorRegion pr cr, MonadIO cr)
                => Shader a Moved pr 
                -> cr (Shader a Compiled pr) 
glShaderCompile = undefined    






    
