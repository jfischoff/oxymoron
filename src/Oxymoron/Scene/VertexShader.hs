{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Oxymoron.Scene.VertexShader where
import Graphics.Rendering.OpenGL.Raw
import Oxymoron.Scene.Shader
import qualified Oxymoron.Description.VertexShader as Desc
import qualified Oxymoron.Description.Shader as Desc
import Data.Singletons
import Control.Monad.IO.Class ( MonadIO, liftIO )

data VertexShader :: Desc.VertexShader -> ShaderState -> * where
    VertexShader :: Sing Desc.VertexShader 
                 -> Shader t s
                 -> VertexShader (Desc.GetVertexShader (GetDesc t)) s 






                     















           
