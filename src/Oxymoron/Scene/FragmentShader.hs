{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Scene.FragmentShader where
import Language.GLSL.Syntax
import Graphics.Rendering.OpenGL.Raw
import Oxymoron.Scene.Resource
import Oxymoron.Scene.Shader
import qualified Oxymoron.Description.FragmentShader as Desc
import qualified Oxymoron.Description.Shader as Desc
import Data.Singletons
import Control.Monad.Trans.Region
import Control.Monad.IO.Class ( MonadIO, liftIO )

data FragmentShader :: Desc.FragmentShader -> ShaderState -> (* -> *) -> * where
    FragmentShader :: Shader t s r 
                 -> FragmentShader (Desc.GetFragmentShader (GetDesc t)) s r