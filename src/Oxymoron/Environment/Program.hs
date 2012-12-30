{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Environment.Program where
import qualified Oxymoron.Description.Shader as Desc
import Data.Singletons
import Foreign.Ptr
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Oxymoron.TypeLevel.Nat

singletons [d| 
    data ProgramState  = ProgramCreated 
                       | VertexAttached   Nat
                       | FragmentAttached Nat
                       | Attached Nat Nat
                       | Linked
                       | Validated
                       
    data Program = Program ProgramState |]
                      

