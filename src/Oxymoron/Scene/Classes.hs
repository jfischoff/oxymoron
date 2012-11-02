{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Scene.Classes where
    
data OpenGLCommand a

class OpenGL m where
   openGL :: OpenGLCommand a -> m a
   
class (Monad m) => GLTransition m a where
   glStep :: a -> m a
   
class IsAllocated (a :: k) where
    