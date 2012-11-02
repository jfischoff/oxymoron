{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
module Oxymoron.Scene.Material where
import Oxymoron.Scene.Program
import Graphics.Rendering.OpenGL.Raw
import Oxymoron.Scene.Classes
import Oxymoron.Scene.Core
import Oxymoron.Scene.Uniform

data Material (a :: Desc.Material) = Material a (ToUniforms a)

{-
data MaterialState = MaterialCreated ProgramState BoundType Liveness
                   | MaterialSetup 
                   | MaterialToredown

data Material :: MaterialState -> * where
    CreateMaterial   :: Program a -> [Uniform b c]                        -> Material (MaterialCreated a b c)
    SetupMaterial    :: Material (MaterialCreated Linked Bound Allocated) -> Material MaterialSetup
    TeardownMaterial :: Material MaterialSetup                            -> Material MaterialToredown
-}

