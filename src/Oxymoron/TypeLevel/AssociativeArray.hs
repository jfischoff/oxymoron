{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, TemplateHaskell #-}
module Oxymoron.TypeLevel.AssociativeArray where
import Data.Singletons    

singletons [d| 
        data AssocArray a b = AssocArray [(a, b)] 
        

        fst :: (a, b) -> a 
        fst (a, b) = a

        snd :: (a, b) -> b
        snd (a, b) = b

    
    |]

type instance ('AssocArray xs) :==: ('AssocArray ys) = 
    If ((xs :==: '[ ]) :&&: (ys :==: '[ ]))
        True
        (If (xs :==: '[ ]) False (
            If (ys :==: '[ ])
                False
                (xs :==: (Reorder ys xs))
                ))
                
type instance '(a, b) :==: '(x, y) = (a :==: x) :&&: (b :==: y) 

--type family Fst (x :: (a,b)) :: a
--type instance Fst '(a,b) = a

--type family Snd (x :: (a,b)) :: b
--type instance Snd '(a,b) = b



-- remove a DimSpec from a list, if it's in the list;
-- returns the new list and possibly the extracted element  
type family Extract (s :: k) (lst :: [k]) :: ([k], Maybe k)
type instance Extract s '[] = '( '[], Nothing)
type instance Extract s (h ': t) =
  If (s :==: h)
   '(t, Just h)
   '(h ': Fst (Extract s t), Snd (Extract s t))

-- reorder 'a' in the order defined by 'b'. 
type family Reorder (a :: [k]) (b :: [k]) :: [k]
type instance Reorder x '[] = x
type instance Reorder x (h ': t) = Reorder' (Extract h x) t

type family Reorder' (scrut :: ([k], Maybe k))
                     (t :: [k])
                     :: [k]
type instance Reorder' '(lst, Nothing) t = Reorder lst t
type instance Reorder' '(lst, Just elt) t = elt ': (Reorder lst t)

type family Lookup (a :: k) (x :: AssocArray a b) :: Maybe b
type instance Lookup x ('AssocArray '[])     = 'Nothing
type instance Lookup x ('AssocArray ('(k, v) ': ys)) = 
    If (x :==: k) 
        ('Just v)
        (Lookup x ('AssocArray ys))
        
type family Insert (a :: k) (b :: l) (x :: AssocArray a b) :: AssocArray k l
type instance Insert k v ('AssocArray x) = 'AssocArray (Insert' k v '[] x)

type family Insert' (a :: k) (b :: l) (p :: [(k,l)]) (n :: [(k,l)]) :: [(k, l)]
type instance Insert' k v xs '[] = ('( k , v) ': xs)
type instance Insert' k v xs ('(k', v') ': ys) = 
    If (k :==: k')
        (xs :++ (('( k, v) ': ys)))
        (Insert' k v ( ( '(k', v') ': xs)) ( ys))

singFst :: (SingRep (a :: k), SingRep (b :: l)) 
        => Sing (t :: (k, l)) 
        -> Sing a
singFst _ = sing

makePair :: (SingRep '( (a :: k), (b :: l))) 
         => Sing a 
         -> Sing b 
         -> Sing ('( a, b) :: (k, l)) 
makePair _ _ = sing


insert' :: (SingRep (key :: k), SingRep (value :: l), SingRep xs,
            SingKind (Any :: l), SingKind (Any :: k), SEq key) 
         => Sing key  
         -> Sing value 
         -> Sing (xs :: [(k, l)]) 
         -> Sing (ys :: [(k, l)])
         -> Sing (Insert' key value xs ys)
insert' key value xs SNil = SCons (makePair key value) xs
insert' k v xs (SCons p ys) = if fromSing (k %==% singFst p) 
        then xs %:++ (SCons (makePair k v) ys)
        else insert' k v (SCons p xs) xs

singletons [d| data Test = Test1
                         | Test2 
                         |]

testKey :: Sing 'Test1
testKey = sing

testValue :: Sing 'Test2
testValue = sing 

testList :: Sing '[ '( 'Test1, 'Test2 )]
testList = sing

--test1 = insert' testKey testValue testList










-- this won't compile
--test2 = test testA testC

