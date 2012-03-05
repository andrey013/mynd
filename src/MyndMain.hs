module Main where

import Control.Monad ( forever,void,when )
import Control.Concurrent ( threadDelay )
import Data.IORef
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Bindings
import Reactive.Banana
import Reactive.Banana.GLFW

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
  
  angle <- newIORef (0.0::GLfloat)
  delta <- newIORef (0.1::GLfloat)
  position <- newIORef (0.0::GLfloat, 0.0::GLfloat)
  
  GLFW.setWindowSizeCallback reshape
  --GLFW.setKeyCallback $ keyboard delta position
  network <- compile $ do
    eKeyPush <- registerKeyboardPush
    --eKeyRelease <- registerKeyboardRelease
    t <- timer 100
    let eSpace  = filterE (== (GLFW.CharKey ' ') ) eKeyPush
        eEsc    = filterE (== GLFW.KeyEsc ) eKeyPush
        
    let
        dmode :: Discrete Int
        dmode = accumD 0 $
                 ((+1) <$ eEsc)
                 
    reactimate $ (\i -> when (i == 4) (void shutdown)) <$> changes dmode
    reactimate $ (\_ -> idle angle delta >> display angle position >> GLFW.swapBuffers) <$> t

  actuate network
  
  GLFW.setWindowCloseCallback shutdown
  
  forever $ do
    --idle angle delta
    --display angle position
    --GLFW.swapBuffers
    threadDelay 1000

