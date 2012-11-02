{-# LANGUAGE GADTs, DataKinds, KindSignatures, RankNTypes, EmptyDataDecls #-}
module Oxymoron.Scene.Program where
import Oxymoron.Scene.Shader 
import qualified Oxymoron.Scene.Shader as S
import Language.GLSL.Syntax
import Graphics.Rendering.OpenGL.Raw
import Oxymoron.Scene.Classes
import Oxymoron.Scene.Core

data ProgramValue (a :: Desc.Program) b c = ProgramValue (ToVertexShader a b) (ToFragmentShader a c)


{-
data ProgramState = ProgramCreated ShaderState ShaderState 
                  | Attached 
                  | Linked
                  | ProgramDestroyed

type ProgramResource a c e = forall b d. Resource a (ProgramValue b c d e)

data Program :: ProgramState -> * where
    ProgramCreate       :: ProgramResource Allocated a b              -> Program (ProgramCreated a b)
    ProgramAttach       :: Program (ProgramCreated Compiled Compiled) -> Program Attached 
    ProgramLink         :: Program Attached                           -> Program Linked
    ProgramDestroy      :: ProgramResource Unallocated a b            -> Program ProgramDestroyed  
    

data InfoProgramState = InfoProgramCreated ProgramState
                      | InfoProgramSetup
                      | InfoProgramDestroyed
                      
data AttributeR
data UniformR
                      
data InfoProgram :: InfoProgramState -> * where
    CreateInfoProgram :: Program a 
                  -> [ResourceMapping AttributeR] 
                  -> [ResourceMapping UniformR] 
                  -> InfoProgramState (InfoProgramState a)
    SetupInfoProgram   :: InfoProgram (InfoProgramState ProgramLink) -> InfoProgram InfoProgramSetup
    DestroyInfoProgram :: InfoProgram InfoProgramSetup               -> InfoProgram InfoProgramDestroyed
-}    

{-    

-- More restrictive then it needs to be


--make a setup function
--that takes a program from any state to the final state

-}
