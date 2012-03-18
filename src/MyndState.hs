module MyndState where

import Data.Interpolable
import Graphics.Rendering.OpenGL
import MyndNet

data MyndState
  = MyndState 
  { angle    :: Interpolable GLfloat
  , delta    :: GLfloat
  , position :: (GLfloat,GLfloat)
  , net      :: MyndNode
  }
