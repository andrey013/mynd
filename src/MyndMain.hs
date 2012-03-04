module Main where

import Control.Monad ( forever )
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Util.Util
import Bindings
import Display

-- |'main' runs the main program
main :: IO ()
main = do
  True <- GLFW.initialize
  let dspOpts = GLFW.defaultDisplayOptions
                  { GLFW.displayOptions_width  = 800
                  , GLFW.displayOptions_height = 600
                  , GLFW.displayOptions_numRedBits   = 8
                  , GLFW.displayOptions_numGreenBits = 8
                  , GLFW.displayOptions_numBlueBits  = 8
                  , GLFW.displayOptions_numAlphaBits = 8
                  , GLFW.displayOptions_numDepthBits = 1
                  -- , GLFW.displayOptions_displayMode = GLFW.Fullscreen
                  }
  True <- GLFW.openWindow dspOpts
  GLFW.setWindowPosition 0 0
  GLFW.setWindowTitle "Hello World"
  GLFW.setWindowSizeCallback reshape
  GLFW.setKeyCallback keyboard
  GLFW.setWindowCloseCallback shutdown
  -- initGL
  forever $ do
    display
    GLFW.swapBuffers

