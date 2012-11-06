{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Scene.Resource where
import Control.Monad.Trans.Region
import Graphics.Rendering.OpenGL.Raw
import Control.Monad                     ( return, liftM )
import Data.Singletons
import Control.Monad.Trans.Region.OnExit ( FinalizerHandle, Finalizer, onExit )


data R a (r :: * -> *) = R a !GLuint !(FinalizerHandle r) 

type SR a r = R (Sing a) r 

instance Dup (R a) where
    dup (R x glId ch) = liftM (R x glId) (dup ch)