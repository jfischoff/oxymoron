{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures, EmptyDataDecls #-}
module Oxymoron.Scene.Image where
import Data.Array.Repa hiding (Any)
import Graphics.Rendering.OpenGL.Raw
import Data.Word
import Data.Int
import Data.Singletons
import Data.Word.Word24
import qualified Oxymoron.Description.Image as Desc
import Oxymoron.Description.Image (PixelFormat(..), 
    InternalFormat(..), PixelComponent(..), ImageTarget(..))
import Oxymoron.Scene.Resource
import Control.Monad.Trans.Region
import Oxymoron.Scene.Resource
import Control.Monad.IO.Class ( MonadIO, liftIO )

singletons [d| data ImageState = Created | Copied |]

data ImageValue :: PixelFormat    ->
                   InternalFormat -> 
                   PixelComponent ->
                   ImageTarget    ->
                   * where
     ImageValue :: Desc.Image a b c d 
           -> Array r DIM2 (ToRepa a c) 
           -> ImageValue a b c d 
           
type family ToRepa (a :: PixelFormat) (b :: PixelComponent) 
type instance ToRepa RED UNSIGNED_BYTE       = Word8
type instance ToRepa RGB UNSIGNED_BYTE_3_3_2 = Word8 -- slowish
type instance ToRepa RGB UNSIGNED_BYTE       = Word24 -- slowish

data Image :: PixelFormat    ->
              InternalFormat -> 
              PixelComponent ->
              ImageTarget    ->
              ImageState     -> 
              (* -> *)       ->
              * where
    Image :: R (ImageValue a b c d) r 
          -> Sing s 
          -> Image a b c d s r
          
glGenTexture :: (AncestorRegion pr cr, MonadIO cr) 
              => ImageValue a b c d 
              -> cr (Image a b c d Created cr)
glGenTexture = undefined

glCopyTexImage2D :: (AncestorRegion pr cr, MonadIO cr) 
                 => Image a b c d Created cr
                 -> cr (Image a b c d Copied cr)
glCopyTexImage2D = undefined                 





