module Oxymoron.Resource.Program where

newtype Ref a = Ref GLuint

data ShaderState     = Created | Moved | Compiled 
data ShaderComponent = Vertex | Fragment

data Shader :: ShaderComponent -> ShaderState -> * where
   Created  :: Ref a -> Shader a Created 
   Moved    :: Ref a -> Shader a Moved   
   Compiled :: Ref a -> Shader a Compiled

type VertexShader   = Shader Vertex
type FragmentShader = Shader Fragment

data ProgramState = Created 
                  | VertexAttached   ShaderState
                  | FragmentAttached ShaderState
                  | Attached ShaderState ShaderState
                  | Linked 
                  | Validated 
                      
data Program :: ProgramState -> * where
   Created          :: Ref Program -> Program Created
   VertextAttached  :: Ref Program 
                    -> VertexShader vs  
                    -> Program (VertexAttached vs)
   FragmentAttached :: Ref Program 
                    -> FragmentShader fs
                    -> Program (FragmentAttached fs)
   Attached         :: Ref Program 
                    -> VertexShader   vs
                    -> FragmentShader fs
                    -> Program (Attached vs fs)
   Linked    :: Ref Program -> Program Linked
   Validated :: Ref Program -> Program Validated
      
data UniformValue      
      
data Uniform = Uniform (Ref Uniform) UniformValue
       deriving(Show, Eq)
 
data AttributeState = Bound | Created 
 
-- This should have all the information necessary for rendering

data PolygonType
data StreamType

data Attribute :: AttributeState -> * -> * where
   ACreated :: Ref Attribute -> a -> Attribute Created a
   ABound   :: Ref Attribute -> a -> Attribute Bound   a

data IndexArray  = IndexArray  PolygonType IndexStreamType (Ptr Word8)
data VertexArray = VertexArray VertexStreamType (Ptr Word8)

data Mesh = Mesh IndexArray [VertexArray]
   
data UniformBlock = UniformBlock [Uniform]

-- I should set up the format that things should be in for efficent rendering

data ProgramGroup = ProgramGroup (Program Validated) [Uniform]

data InstanceGroup = InstanceGroup Mesh [Program Validated]

















