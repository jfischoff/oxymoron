{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, KindSignatures #-}
module Oxymoron.Description.Image where
import Data.Singletons

singletons [d|
    -- TODO add all of the enumerations
    data PixelFormat        = RED | RG | RGB | BGR | RGBA | BGRA      
    data InternalFormat     = IDEPTH_COMPONENT | IRGBA | IGL_COMPRESSED_RGB
    data PixelComponent = UNSIGNED_BYTE | UNSIGNED_BYTE_3_3_2 
        | UNSIGNED_SHORT_4_4_4_4 | FLOAT
    data ImageTarget        = TEXTURE_2D | TEXTURE_1D_ARRAY
    
    -- Not sure if these are right.
    -- More considered with the structure for right now
    isValid :: PixelFormat -> InternalFormat -> PixelComponent -> ImageTarget -> Bool
    isValid RED  IDEPTH_COMPONENT   UNSIGNED_BYTE TEXTURE_2D = True
    isValid RED  IDEPTH_COMPONENT   FLOAT         TEXTURE_2D = True
    isValid RG   IDEPTH_COMPONENT   UNSIGNED_BYTE TEXTURE_2D = True
    isValid RG   IDEPTH_COMPONENT   FLOAT         TEXTURE_2D = True
    isValid RGB  IGL_COMPRESSED_RGB FLOAT         TEXTURE_2D = True
    --TODO finish writing this function ... and write correctly
    
    |]
    
data Image :: PixelFormat 
           -> InternalFormat 
           -> PixelComponent 
           -> ImageTarget -> * where
    Image :: (IsValid a b c d ~ 'True)
          => SPixelFormat a 
          -> SInternalFormat b 
          -> SPixelComponent c 
          -> SImageTarget d 
          -> Image a b c d