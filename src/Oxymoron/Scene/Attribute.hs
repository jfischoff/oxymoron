{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Scene.Attribute where
import qualified Oxymoron.Description.Attribute as Desc
import Graphics.Rendering.OpenGL.Raw
import Data.Singletons
import Oxymoron.Scene.Resource
import Oxymoron.Description.Program
import Control.Monad.Trans.Region
import Control.Monad.IO.Class ( MonadIO, liftIO )

singletons [d| 
    data AttributeState = Created | Bound 
    data AttributeInfo = AttributeInfo AttributeState Desc.Attribute
    
     |]

data Attribute :: AttributeState -> (* -> *) -> Desc.Attribute -> * where
     CreateAttribute :: Sing ('AttributeInfo a c) -> Attribute 'Created b c
     BindAttribute   :: SR ('AttributeInfo a c) b -> Attribute 'Bound b c


--glGetAttribLocation :: (AncestorRegion pr cr, MonadIO cr) 
--                    => SR (Program as us vs) Linked pr -> Sing name
--                    -> cr (SR (FromJust (FindAttr name as)) cr a)
--glGetAttribLocation = undefined



{-
data List a = List [a]

data PromotedType = PromotedType

data FancyKindedType :: PromotedType -> * where
    MakeItHappen :: FancyKindedType 'PromotedType
    
type FancyList = List (FancyKindedType 
-}


--the attribute resource is a child of the progR ... i think
--type AttributeResource progR attrR = Resource a (r :: * -> *)

--glGetAttribLocation :: Resource Program pr -> pr (Resource Attribute pr)
--glVertexAttribPointer :: Resource Attribute pr -> Stream -> pr ()
        
