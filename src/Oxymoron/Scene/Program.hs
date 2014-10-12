{-# LANGUAGE PolyKinds, DataKinds, TemplateHaskell, TypeFamilies,
    GADTs, TypeOperators, RankNTypes, FlexibleContexts, UndecidableInstances,
    FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses,
    OverlappingInstances, InstanceSigs #-}
module Oxymoron.Scene.Program where
import Oxymoron.Scene.Shader 
import Graphics.Rendering.OpenGL.Raw
import Oxymoron.Scene.VertexShader 
import Oxymoron.Scene.FragmentShader
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Oxymoron.Description.Program      as Desc
import qualified Oxymoron.Description.VertexShader as Desc
import qualified Oxymoron.Description.FragmentShader as Desc
import qualified Oxymoron.Description.Attribute    as Desc
import qualified Oxymoron.Description.Uniform      as Desc
import qualified Oxymoron.Description.Varying      as Desc
import Data.Singletons
import Data.Singletons.Extras.Set as S




data Program :: Set Desc.Attribute
             -> Set Desc.Uniform 
             -> Set Desc.Uniform 
             -> Set Desc.Varying
             -> ProgramState 
             -> * where
   PCreated  :: GLuint -> Program () ProgramCreated
   PVertexAttached :: GLuint
                   -> VertexShader   ('Desc.VertexShader a b d) vs
                   -> Program a b S.Empty d ('VertexAttached vs)
   PFragmentAttached :: GLuint
                     -> FragmentShader ('Desc.FragmentShader d c) fs
                     -> Program S.Empty S.Empty c d ('FragmentAttached vs)
   PAttached :: VertexShader   ('Desc.VertexShader a b d) vs
             -> FragmentShader ('Desc.FragmentShader d c) fs
             -> Program        a b c d ('Attached vs fs)
   PLinked :: 
   
-- I'm starting to get a new idea
-- which is that there is the description of the vertex shader 
-- the actual data
-- and the there is the resource ids
-- which is something else
-- The resourse ids have all the state
-- and they are what is rendered
-- this is also important because we can drop stuff that is tnoth needed

             

--glUseProgram :: MonadIO pr
--             => Program a b c d 'Linked pr
--             -> RegionT s pr (RegionT s pr)
--glUseProgram = undefined 

{-
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


-}










