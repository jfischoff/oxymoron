{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, TemplateHaskell #-}
module Oxymoron.Regions.Program where    
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Graphics.Rendering.OpenGL.Raw (GLint, GLenum, GLsizei, GLchar, GLint)
import Control.Monad.Trans.Region
import Control.Monad.Trans.Region.OnExit
import Oxymoron.Regions.Resource
import Control.Monad.IO.Class (MonadIO, liftIO)
import Oxymoron.Regions.Shader
import Control.Lens

glCreateProgram :: (MonadIO pr, ProgramResource programResource)
                => RegionT s pr (programResource (RegionT s pr))
glCreateProgram = do 
    progId <- liftIO $ GL.glCreateProgram 
    fin    <- onExit $ GL.glDeleteProgram progId
    return $ programResource progId fin 

glAttachShader :: (AncestorRegion pr cr, MonadIO cr, 
                  ProgramResource programResource,
                  ShaderResource shaderResource) 
               => programResource pr -> shaderResource pr -> cr ()
glAttachShader progHandle shaderHandle = liftIO $ 
    GL.glAttachShader (_resourceId . unprogramResource $ progHandle) 
                      (_resourceId . unshaderResource  $ shaderHandle)

glLinkProgram :: (AncestorRegion pr cr, MonadIO cr,
                 ProgramResource programResource)
              => programResource pr 
              -> cr ()
glLinkProgram progHandle = liftIO $ GL.glLinkProgram 
    (_resourceId . unprogramResource $ progHandle)

--I need to be able to change the state monad to different type
glUseProgram :: (AncestorRegion pr cr, 
                 MonadIO cr, 
                 ProgramResource programResource)
             => programResource pr
             -> cr ()
glUseProgram progHandle = do 
    liftIO $ GL.glUseProgram (_resourceId . unprogramResource $ progHandle)

















