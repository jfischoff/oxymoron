{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures, TemplateHaskell #-}
module Oxymoron.Regions.Resource where
import Control.Monad.Trans.Region
import Graphics.Rendering.OpenGL.Raw
import Control.Monad                     ( return, liftM )
import Data.Singletons
import Control.Monad.Trans.Region.OnExit ( FinalizerHandle, Finalizer, onExit )
import Control.Lens


singletons [d| data Program     = Program
               data Shader      = Shader 
               data Image       = Image
               |]
               
data R a (r :: * -> *) = R { 
        _resourceId        :: GLuint,
        _resourceFinalizer :: !(FinalizerHandle r) 
    }
    
makeLenses ''R

class ShaderResource (handle :: (* -> *) -> *) where
    unshaderResource :: handle r -> R Shader r 
    shaderResource :: GLuint -> FinalizerHandle r -> handle r

class ProgramResource (handle :: (* -> *) -> *) where
    unprogramResource :: handle r -> R Program r
    programResource :: GLuint -> FinalizerHandle r -> handle r

class ImageResource (handle :: (* -> *) -> *) where
    unimageResource :: handle r -> R Image r
    imageResource :: GLuint -> FinalizerHandle r -> handle r

instance Dup (R a)  where
    dup (R glId ch) = liftM (R glId) (dup ch)








