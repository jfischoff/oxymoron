{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, TemplateHaskell #-}
module Oxymoron.Regions.Image where
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Graphics.Rendering.OpenGL.Raw (GLint, GLenum, GLsizei, GLchar, GLint,
    gl_TEXTURE_CUBE_MAP_POSITIVE_X, gl_TEXTURE_2D)
import Control.Monad.Trans.Region
import Control.Monad.Trans.Region.OnExit
import Oxymoron.Regions.Resource
import Control.Monad.IO.Class (MonadIO, liftIO)
import Oxymoron.Regions.Shader
import Control.Lens
import Data.Singletons
import Control.Monad.State
import Oxymoron.Class


singletons [d| 
               data ImageTarget = GL_TEXTURE_2D
                                | GL_TEXTURE_CUBE_MAP_POSITIVE_X 
                                | GL_TEXTURE_CUBE_MAP_NEGATIVE_X
                                | GL_TEXTURE_CUBE_MAP_POSITIVE_Y
                                | GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
                                | GL_TEXTURE_CUBE_MAP_POSITIVE_Z
                                | GL_TEXTURE_CUBE_MAP_NEGATIVE_Z 
             |]
             
instance ToGLenum (Sing (a :: ImageTarget)) where
   toGLenum SGL_TEXTURE_2D = gl_TEXTURE_2D
   toGLenum SGL_TEXTURE_CUBE_MAP_POSITIVE_X = gl_TEXTURE_CUBE_MAP_POSITIVE_X

glGenTextures :: (MonadIO pr, ImageResource imageResource) 
              => GLsizei 
              -> RegionT s pr [imageResource (RegionT s pr)]
glGenTextures count = undefined

glBindTexture :: (MonadIO cr, 
                  AncestorRegion pr cr,
                  ImageResource imageResource) 
              => Sing (a :: ImageTarget) -> imageResource pr -> cr ()
glBindTexture target handle = liftIO $ 
    GL.glBindTexture (toGLenum target)  (_resourceId . unimageResource $ handle)
    

    







