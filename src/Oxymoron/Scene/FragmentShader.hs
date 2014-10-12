{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Scene.FragmentShader where
import Graphics.Rendering.OpenGL.Raw
import Oxymoron.Scene.Shader
import qualified Oxymoron.Description.FragmentShader as Desc
import qualified Oxymoron.Description.Shader as Desc
import Data.Singletons

data FragmentShader :: Desc.FragmentShader -> ShaderState -> * where
    FragmentShader :: Shader t s 
                 -> FragmentShader (Desc.GetFragmentShader (GetDesc t)) s