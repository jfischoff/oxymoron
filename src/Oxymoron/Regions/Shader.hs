{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, TemplateHaskell #-}
module Oxymoron.Regions.Shader where
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Graphics.Rendering.OpenGL.Raw (GLint, GLenum, GLsizei, GLchar, GLint)
import Data.Singletons
import Control.Monad.Trans.Region
import Control.Monad.Trans.Region.OnExit
import Oxymoron.Regions.Resource
import Control.Monad.IO.Class ( MonadIO, liftIO )
--import qualified Graphics.Rendering.OpenGL.Raw.Core32 as GL
--import Graphics.Rendering.OpenGL.Raw.Core32 (GLenum)
import Foreign.Ptr
import Control.Lens

-- I think the trick here is to have a type class for the handle(s)
-- I should make sure that all of the calls have a handle with a regions
-- don't worry about the states

glCreateShader :: (MonadIO pr, ShaderResource shaderResource)
               => GLenum
               -> RegionT s pr (shaderResource (RegionT s pr))
glCreateShader x = do
    shaderId <- liftIO $ GL.glCreateShader x
    fin      <- onExit $ GL.glDeleteShader shaderId
    return $ shaderResource shaderId fin

glShaderSource :: (AncestorRegion pr cr, MonadIO cr, ShaderResource shaderResource) 
               => shaderResource pr -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> cr ()
glShaderSource h count sources lengths = do 
    liftIO $ GL.glShaderSource (_resourceId . unshaderResource $ h) 
        count sources lengths
    
glCompileShader :: (AncestorRegion pr cr, MonadIO cr, ShaderResource shaderResource)
                => shaderResource pr 
                -> cr () 
glCompileShader = liftIO . GL.glCompileShader . _resourceId . unshaderResource     






    
