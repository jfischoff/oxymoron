module Oxymoron.Scene.Rendering where
import Control.Monad.Trans.Region
import Control.Monad                     ( return, liftM )
import Data.Singletons
import Control.Monad.Trans.Region.OnExit ( FinalizerHandle, Finalizer, onExit )
import Oxymoron.Scene.Resource
import Oxymoron.Scene.Shader
import Oxymoron.Scene.VertexShader
import Oxymoron.Scene.Program
import Control.Monad


-- TODO 
-- The current observation is that there is something like a GLHandle
-- that doesn't have all the fullblown information of the GLTypes
-- Another way to look at this is to see the regions as seperate from the type information
-- that I am setting up
-- so the plan would be to make a thin layer over the 
-- the gltypes that just ensured that the resources are allocated 
-- and that they have the write state
-- but doesn't have any of the heavy type information
-- and then combine the two



main :: IO ()
main = do
    let vShaderInfo = undefined :: ShaderInfo ('ShaderType a 'Sourced 'Vertex)
        fShaderInfo = undefined :: ShaderInfo ('ShaderType a 'Sourced 'Fragment)
        --progInfo    = undefined :: 
        setupShader = glShaderCompile <=< glShaderSource <=< glCreateShader
    runRegionT $ do 
        vShad <- setupShader vShaderInfo
        fShad <- setupShader fShaderInfo
        
        prog <- glCreateProgram 
        
        return ()
--        
    return ()






