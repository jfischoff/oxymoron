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

testVertexShader1 :: Sing ('VertexShader '[] '[] '[ 'Varying Color VFloat, 'Varying Position VInt] )
testVertexShader1 = sing

testFragmentShader1 :: Sing ('FragmentShader '[ 'Varying Position VInt, 'Varying Color VFloat ] '[])
testFragmentShader1 = sing

testProgram1 = Program testVertexShader1 testFragmentShader1

testVertexShader2 :: Sing ('VertexShader '[] '[] '[ 'Varying Color VFloat ])
testVertexShader2 = sing

testFragmentShader2 :: Sing ('FragmentShader '[ 'Varying Position VInt, 'Varying Color VInt] '[])
testFragmentShader2 = sing

--testProgram2 = Program testVertexShader1 testFragmentShader2
singletons [d| data TestValue = Test1
                              | Test2
                              | Test3
                         deriving(Eq, Show)
                         
               data TestKey = Key1
                             | Key2
                             | Key3
                             deriving(Eq, Show)
                         
                         |]


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


test :: ((a :==: b) ~ True) => Sing (a :: AssocArray k v) -> Sing (b :: AssocArray k v) -> Int
test = undefined



testA :: Sing ('AssocArray '[ '( 'Key1, 'Test3 ),
                             '( 'Key2, 'Test2 )])
testA = undefined
                          
testB :: Sing ('AssocArray '[ '( 'Key2, 'Test2),
                             '( 'Key1, 'Test3)])
testB = undefined

testC :: Sing ('AssocArray '[ '( 'Key2, 'Test3) ])
testC = undefined


-- Order is different but it still compiles
test1 = test testA testB

testKey :: Sing 'Key1
testKey = sing

testValue :: Sing ('Just 'Test1)
testValue = sing

findTester :: (Lookup key array ~ Just value) 
           => Sing (key :: k) 
           -> Sing (array :: AssocArray k v)
           -> Sing (value :: v)
findTester = undefined      

findTest = findTester testKey testA
