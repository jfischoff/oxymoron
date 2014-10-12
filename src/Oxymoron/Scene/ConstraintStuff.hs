{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE RankNTypes #-}


module Oxymoron.Scene.Varying where
import Data.Constraint
import GHC.Prim

data ConstraintC  
   = FunctorC 
   | MonadC  
   | ShowC 

testDict = Dict ; testDict :: Dict (Functor Maybe)

test :: SMaybeT (JustT (ConstraintE 'FunctorC (Functor Maybe)))
test = encode testDict

-- MaybeT

data JustT a
data NothingT 

-- This is my wacky idea

data SMaybeT :: * -> * where
   SJust    :: a -> SMaybeT (JustT a)
   SNothing :: SMaybeT NothingT
   
instance Show a => Show (SMaybeT (JustT a)) where
   show e = case e of
      SJust x  -> "SJust " ++ show x
   

data ConstraintE :: ConstraintC -> Constraint -> * where
   FunctorE :: c ~ Functor a => ConstraintE FunctorC c
   MonadE   :: c ~ Monad   a => ConstraintE MonadC   c
   
instance Show (ConstraintE a c) where
   show e = case e of
      FunctorE -> "FunctorE"
      MonadE -> "MonadE"

data Tagged1 a = Tagged1

class ToConstraintC (i :: Constraint) o where 
   constraint :: Tagged1 i -> SMaybeT o 
   
instance (Pred i flag, ToConstraintC' flag i o) => ToConstraintC i (o :: *) where
   constraint Tagged1 = constraint' (Tagged2 :: Tagged2 flag i)

class Pred (a :: Constraint)  (flag :: Bool) | a -> flag where {}
instance (flag ~ False) => Pred a flag
instance Pred (Functor a) True

data Tagged2 a b = Tagged2

class ToConstraintC' (t :: Bool) (i :: Constraint) (o ::  *)  where 
   constraint' :: Tagged2 t i -> SMaybeT o
   
instance ToConstraintC' True (Functor a) (JustT (ConstraintE 'FunctorC (Functor a))) where
   constraint' Tagged2 = SJust FunctorE 
   
instance ToConstraintC' False a NothingT where
   constraint' Tagged2 = SNothing

encode :: forall (c :: Constraint) (o :: *) . 
          (ToConstraintC c o) 
        => Dict c -> SMaybeT o 
encode Dict = constraint (Tagged1 :: Tagged1 c)