{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, TemplateHaskell #-}
module Oxymoron.Environment.Internal where
import Oxymoron.Regions (ShaderResource, R (..))
import qualified Oxymoron.Regions as R
import Data.Singletons 
import Graphics.Rendering.OpenGL.Raw
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Oxymoron.TypeLevel.AssociativeArray
import Control.Monad.State
import Oxymoron.Environment.Shader
import Oxymoron.Environment.Program
import Oxymoron.Environment.Image
import Control.Monad.Trans.Region
import Control.Monad.Trans.Region.OnExit
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Oxymoron.Description.Image as Desc
import Oxymoron.TypeLevel.Nat
import Oxymoron.TypeLevel.Symbol hiding (Insert)
import qualified Oxymoron.Description.Shader as Desc
import Oxymoron.Class

singletons [d| data Named a = Named Symbol a |]

data NamedShader :: Symbol -> (* -> *) -> * where
    NamedShader :: Sing a -> R Shader s -> NamedShader a s

data Env :: Maybe Program 
         -> Maybe (Desc.ImageTarget, Image) 
         -> AssocArray Symbol Image
         -> AssocArray Symbol Shader 
         -> AssocArray Symbol Program
         -> * where 
    Env :: {
            currentProgram :: Sing a,
            currentImage   :: Sing b,
            images         :: Sing c,
            shaders        :: Sing d,
            programs       :: Sing e
        } -> Env a b c d e
        
type family FromJust (a :: Maybe k) :: k
type instance FromJust ('Just x) = x

type family HasSource (a :: Maybe Shader) :: Bool
type instance HasSource 'Nothing  = 'False
type instance HasSource ('Just ('ShaderMoved x   )) = 'True 
type instance HasSource ('Just ('ShaderCompiled x)) = 'True

type family LookupShaderType (a :: k) (x :: AssocArray a Shader) :: ShaderType
type instance LookupShaderType k a = GetShaderType (FromJust (Lookup k a))

-- instead 




insertShader :: Sing (name   :: Symbol) 
             -> Sing (shader :: Shader) 
             -> Env a b c ('AssocArray xs :: AssocArray Symbol Shader) e 
             -> Env a b c ('AssocArray ('( name, shader) ': xs)) e 
insertShader name value (Env a b c (SAssocArray xs) e) = undefined
    --Env a b c (SAssocArray undefined) e
             --(SAssocArray ((name, value) : xs))

glCreateShader :: (MonadIO pr, ToGLenum (Sing (typ :: ShaderType)),
                    SingI (typ :: ShaderType))
                => Env a b c ('AssocArray xs) e
                -> Sing (name :: Symbol)
                -> Sing (typ :: ShaderType)
                -> RegionT s pr 
                    (Env a b c ('AssocArray ('(name, 'ShaderCreated typ) ': xs)) e,
                    NamedShader name (RegionT s pr))
glCreateShader env name typ = do
    shaderId <- liftIO $ GL.glCreateShader (toGLenum typ)
    fin      <- onExit $ GL.glDeleteShader shaderId

    return $ (insertShader name (SShaderCreated typ) env, NamedShader name (R shaderId fin))

glShaderSource :: (AncestorRegion pr cr,
                   MonadIO cr,
                   (Lookup id d) ~ 'Just z,
                   (d' :==: (Insert id ('ShaderMoved (LookupShaderType id d)) d)) ~ 'True) 
               => Env a b c d e
               -> NamedShader id pr
               -> cr (Env a b c d' e)
glShaderSource x = undefined

{-do 
    let shaderId'      = x^.shaderId
        sourcesCount   = x^.sources.count
        sourcesStrings = x^.sources.strings
        sourcesLengths = x^.sources.lengths
        
    liftIO $ GL.glShaderSource shaderId' sourcesCount sourcesStrings sourcesLengths
    return $ set shaderState SMoved x
-}



glShaderCompile :: (AncestorRegion pr cr, MonadIO cr, 
                    HasSource(Lookup id d) ~ 'True,
                    (d' :==: (Insert id ('ShaderCompiled shader) d)) ~ 'True,
                    ((LookupShaderType id d) :==: (GetShaderDescType shader)) ~'True)
                => Env a b c d e
                -> NamedShader id pr
                -> Sing (shader :: Desc.Shader)
                -> cr (Env a b c d' e) 
glShaderCompile = undefined
















