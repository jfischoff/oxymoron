{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Environment.Shader where
import qualified Oxymoron.Description.Shader as Desc
--import Oxymoron.Description.Shader(Shader(V, F))
import Data.Singletons
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Oxymoron.Class

singletons [d| 
    --data ShaderState = ShaderCreated | Moved | Compiled
    data ShaderType  = Vertex | Fragment
    data Shader = ShaderCreated ShaderType
                | ShaderMoved ShaderType
                | ShaderCompiled Desc.Shader
    
    --getShaderState :: Shader -> ShaderState
    --getShaderState (Shader x _) = x
    
    
    getShaderType :: Shader -> ShaderType
    getShaderType     (ShaderCreated x)  = x
    getShaderMoved    (ShaderMoved x)    = x
--    getShaderCompiled (ShaderCompiled x) = getShaderDescType x    
    
    |]
    
data Sources = Sources {
        _count   :: GL.GLsizei,
        _strings :: Ptr (Ptr GL.GLchar), 
        _lengths :: Ptr GL.GLint
    }
    
    
type family GetShaderDescType (x :: Desc.Shader) :: ShaderType
type instance GetShaderDescType (Desc.V x) = 'Vertex
type instance GetShaderDescType (Desc.F x) = 'Fragment


instance ToGLenum (Sing ShaderType) where




