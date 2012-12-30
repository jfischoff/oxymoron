{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Environment.Image where
import qualified Oxymoron.Description.Image as Desc
import Data.Singletons
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL

singletons [d| 
    
    data Image = CreatedImage 
               | CopiedImage Desc.PixelFormat 
                             Desc.InternalFormat 
                             Desc.PixelComponent 
                             Desc.ImageTarget
    |]
    
