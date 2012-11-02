{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances #-}
module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Singletons
import Oxymoron

main = defaultMain [] 

testVertexShader0 :: Sing ('VertexShader '[] '[] '[])
testVertexShader0 = sing

testFragmentShader0 :: Sing ('FragmentShader '[] '[])
testFragmentShader0 = sing

testProgram0 = Program testVertexShader0 testFragmentShader0

testVertexShader1 :: Sing ('VertexShader '[] '[] 
    ('Varying Color VFloat ': 'Varying Position VInt ': ('[] :: [Varying])))
testVertexShader1 = sing

testFragmentShader1 :: Sing ('FragmentShader 
    ('Varying Position VInt ': 'Varying Color VFloat ': ('[] :: [Varying])) '[])
testFragmentShader1 = sing

testProgram1 = Program testVertexShader1 testFragmentShader1

testVertexShader2 :: Sing ('VertexShader '[] '[] '[ 'Varying Color VFloat ])
testVertexShader2 = sing

testFragmentShader2 :: Sing ('FragmentShader 
    ('Varying Position VInt ': 'Varying Color VInt ': ('[] :: [Varying])) '[])
testFragmentShader2 = sing

--testProgram2 = Program testVertexShader1 testFragmentShader2



testMesh0 :: Sing (ExMesh ('[] :: [Attribute]) ': 
                        ('[] :: [*])) 
testMesh0 = undefined    

-- Sing (''Mesh ('IndexArray 'IUnsignedByte 'TRIANGLES) 
--        ['Attribute  "" 'VTByte 'C2])
testMesh1 :: Sing (ExMesh  
                ('Attribute ('Symbol ('[] :: [AChar] )) 
                'VTByte 'C2 ': ('[] :: [Attribute])) ': ('[] :: [*])) 
testMesh1 = undefined


testMaterial0 :: Material ('[] :: [Attribute]) ('[] :: [Uniform]) ('[] :: [Varying])
testMaterial0 = undefined

testRenderable0 = Renderable testMesh0 testMaterial0
--testRenderable1 = Renderable testMesh1 testMaterial0
