{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Scene.Core where
import Graphics.Rendering.OpenGL.Raw
import Data.Singletons
    
singletons [d| data Liveness = Allocated | Unallocated |]

data Resource :: Liveness -> * -> * where
    U :: a          -> Resource Unallocated a
    B :: GLint -> a -> Resource Allocated   a
    
singletons [d| data BoundType = Bound | Unbound |]   
    
data Reference :: BoundType -> * -> * where
    NU :: String -> a -> Reference Unbound a
    NB :: GLint  -> a -> Reference Bound   a
    
data ResourceMapping a = ResourceMapping String GLint
    
class IResource a where
    allocate   :: Resource Unallocated a -> Resource Allocated   a
    deallocate :: Resource Allocated   a -> Resource Unallocated a
    
-- Heterogenous list
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList t -> HList (a ': t)

    

    

