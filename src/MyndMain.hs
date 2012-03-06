module Main where

import Control.Monad ( forever,void,when )
import Control.Concurrent ( threadDelay )
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Bindings
import Reactive.Banana
import Reactive.Banana.GLFW

data MyndState
  = MyndState 
    {
      angle :: GLfloat,
      delta :: GLfloat,
      position :: (GLfloat,GLfloat)
    }

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
  GLFW.setWindowTitle "Hello World"
  
  --angle <- newIORef (0.0::GLfloat)
  --delta <- newIORef (0.1::GLfloat)
  --position <- newIORef (0.0::GLfloat, 0.0::GLfloat)
  
  network <- compile $ do
    eKeyPush <- keyboardPress
    
    eResize <- windowResize
    eClose <- windowClose
    
    t <- timer 20
    
    let
        eSpace  = filterE (== (GLFW.CharKey ' ') ) eKeyPush
        eEsc    = filterE (== GLFW.KeyEsc ) eKeyPush
        ePlus   = filterE (== (GLFW.CharKey '=') ) eKeyPush
        eMinus  = filterE (== (GLFW.CharKey '-') ) eKeyPush
        
        state :: Behavior MyndState
        state = stepper (MyndState 0 0 (0,0)) $ (MyndState 2 0 (0,0)) <$ eSpace
        
        eangle :: Event GLfloat
        eangle = ((\y -> angle y) <$> state) <@ t
        {-
        bangle :: Behavior GLfloat
        eangle :: Event GLfloat
        (eangle, bangle) = mapAccum (0::GLfloat) . fmap (\f x -> (f x,f x)) $
            ((+1) <$ ePlus) `union` ((subtract 1) <$ eMinus) `union` ((+0.1) <$ t)
            
        angle :: Discrete GLfloat
        angle = accumD (0::GLfloat) $
                ((+1) <$ ePlus) `union` ((subtract 1) <$ eMinus) `union` (id <$ t)
-}
        position :: Discrete (GLfloat,GLfloat)
        position = accumD (0::GLfloat,0::GLfloat) $
                   (id <$ ePlus) `union` (id <$ eMinus)
    
    reactimate $ reshape <$> eResize
    reactimate $ shutdown <$ eEsc
    reactimate $ shutdown <$ eClose
    reactimate $ ({-idle angle delta >> -}\a -> (display a) >> GLFW.swapBuffers) <$> eangle

  actuate network
  
  forever $ do
    threadDelay 1000000

