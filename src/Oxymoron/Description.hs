-- | This module provides OpenGL types that maintain important invariants by 
--   construction through the use of singleton types 
--   (using the singleton library). 
--   For example a shader program must contain a vertex shader with output 
--   that matches a fragment shaders input. The parameters are matched up by name,
--   and inorder is not important. For example the following with type check
-- 
-- @  
--   testVertexShader1 :: Sing ('VertexShader '[] '[] ('Varying Color VFloat ': ('[] :: [Varying])))
--   testVertexShader1 = sing
--
--   testFragmentShader1 :: Sing ('FragmentShader ('Varying Color VFloat ': ('[] :: [Varying])) '[])
--   testFragmentShader1 = sing
--
--   testProgram1 = Program testVertexShader1 testFragmentShader1
-- @ 
--   
--   But the following will not
--   
-- @ 
--   
--   testFragmentShader2 :: Sing ('FragmentShader ('Varying Color VInt ': ('[] :: [Varying])) '[])
--   testFragmentShader2 = sing
--   
--   testProgram2 = Program testVertexShader1 testFragmentShader2
-- @
--   
module Oxymoron.Description (
    Attribute(..),
    Color,
    VInt,
    VFloat,
    ValueType(..),
    CoordinateType(..),
    AChar(..),
    FragmentShader(..),
    Material(..),
    Mesh(..),
    Program(..),
    Renderable(..),
    Symbol(..),
    Uniform(..),
    Varying(..),
    VertexShader(..),
    ExMesh(..)
    ) where
import Oxymoron.Description.Attribute
import Oxymoron.Description.FragmentShader
import Oxymoron.Description.Material
import Oxymoron.Description.Mesh
import Oxymoron.Description.Program
import Oxymoron.Description.Renderable
import Oxymoron.Description.Symbol
import Oxymoron.Description.Uniform
import Oxymoron.Description.Varying
import Oxymoron.Description.VertexShader














