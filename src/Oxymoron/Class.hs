module Oxymoron.Class where
import Graphics.Rendering.OpenGL.Raw (GLenum)

class ToGLenum a where
    toGLenum :: a -> GLenum

