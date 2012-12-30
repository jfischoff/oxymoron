{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, InstanceSigs #-}
module Oxymoron.Scene.Program where
import Oxymoron.Scene.Shader 
import qualified Oxymoron.Scene.Shader as S
import Language.GLSL.Syntax
import Graphics.Rendering.OpenGL.Raw
import Control.Monad.Trans.Region
import Oxymoron.Scene.Resource
import qualified Oxymoron.Scene.VertexShader as S
import qualified Oxymoron.Scene.FragmentShader as S
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Oxymoron.Description.Program as Desc
import qualified Oxymoron.Description.VertexShader as Desc
import Oxymoron.Description.VertexShader 
import Oxymoron.Description.FragmentShader 
import qualified Oxymoron.Description.Attribute as Desc
import qualified Oxymoron.Description.Uniform as Desc
import qualified Oxymoron.Description.Varying as Desc
import Data.Singletons


singletons [d| 
    data ProgramState = ProgramCreated 
                      | VertexAttached   ShaderState
                      | FragmentAttached ShaderState
                      | Attached ShaderState ShaderState
                      | Linked 
                      | Validated |]

data Program :: [Desc.Attribute] 
             -> [Desc.Uniform] 
             -> [Desc.Uniform] 
             -> [Desc.Varying]
             -> ProgramState
             -> (* -> *) 
             -> * where
        CreatedProgram :: ProgramResource a b c d r
                       -> Program a b c d 'ProgramCreated r
        AttachVertex :: ProgramResource a b c d r
                       -> S.VertexShader ('VertexShader a b d) vs r
                       -> Program a b c d ('VertexAttached vs) r
        AttachFragment :: ProgramResource a b c d r
                       -> S.FragmentShader ('FragmentShader d c) fs r
                       -> Program a b c d ('VertexAttached fs) r
        Attach         :: ProgramResource a b c d r
                       -> S.VertexShader ('VertexShader a b d) vs r
                       -> S.FragmentShader ('FragmentShader d c) fs r
                       -> Program a b c d ('Attached vs fs) r

type ProgramResource a b c d r = R (Desc.Program a b c d) r

--glUseProgram :: MonadIO pr
--             => Program a b c d 'Linked pr
--             -> RegionT s pr (RegionT s pr)
--glUseProgram = undefined 

glCreateProgram :: MonadIO cr
                => Desc.Program a b c d
                -> RegionT s pr (Program a b c d 'ProgramCreated (RegionT s pr))
glCreateProgram = undefined


-- Should make this a type class
-- TODO this can actually be a function
class AttachShader a b c where
    glAttachShader :: (AncestorRegion pr cr, MonadIO cr)
                   => a pr
                   -> b pr 
                   -> cr (c pr)
                   
instance AttachShader (Program a b c d 'ProgramCreated) 
    (S.VertexShader ('VertexShader a b d) s) 
    (Program a b c d ('VertexAttached s)) where
    glAttachShader :: (AncestorRegion pr cr, MonadIO cr)
                   => Program a b c d 'ProgramCreated pr
                   -> S.VertexShader ('VertexShader a b d) s pr
                   -> cr (Program a b c d ('VertexAttached s) pr)
    glAttachShader = undefined


instance AttachShader (Program a b c d ('VertexAttached vs)) 
    (S.FragmentShader ('FragmentShader d c) fs) 
    (Program a b c d ('Attached vs fs)) where
    glAttachShader :: (AncestorRegion pr cr, MonadIO cr)
                   => Program a b c d ('VertexAttached vs) pr
                   -> S.FragmentShader ('FragmentShader d c) fs pr
                   -> cr (Program a b c d ('Attached vs fs) pr)
    glAttachShader = undefined

instance AttachShader (Program a b c d 'ProgramCreated) 
    (S.FragmentShader ('FragmentShader d c) s) 
    (Program a b c d ('FragmentAttached s)) where
    glAttachShader :: (AncestorRegion pr cr, MonadIO cr)
                   => Program a b c d 'ProgramCreated pr
                   -> S.FragmentShader ('FragmentShader d c) s pr
                   -> cr (Program a b c d ('FragmentAttached s) pr)
    glAttachShader = undefined
    
instance AttachShader (Program a b c d ('FragmentAttached fs)) 
    (S.VertexShader ('VertexShader a b d) vs) 
    (Program a b c d ('Attached vs fs)) where
    glAttachShader :: (AncestorRegion pr cr, MonadIO cr)
                   => Program a b c d ('FragmentAttached fs) pr
                   -> S.VertexShader ('VertexShader a b d) vs pr
                   -> cr (Program a b c d ('Attached vs fs) r)
    glAttachShader = undefined

glLinkProgram :: (AncestorRegion pr cr, MonadIO cr) 
              => Program a b c d ('Attached 'Compiled 'Compiled) pr 
              -> cr (Program a b c d 'Linked pr)
glLinkProgram = undefined

glValidateProgram :: (AncestorRegion pr cr, MonadIO cr) 
                  => Program a b c d 'Linked pr
                  -> cr (Program a b c d 'Validated pr)
glValidateProgram = undefined













