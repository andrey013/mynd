module Main where

import Control.Monad ( forever,void,when,liftM2 )
import Control.Concurrent ( threadDelay )
import Data.Time
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Bindings
import Reactive.Banana
import Reactive.Banana.GLFW

data MyndState
  = MyndState 
    {
      angle :: GLfloat
     ,delta :: GLfloat
     ,position :: (GLfloat,GLfloat)
     --,myndTime :: MyndTime
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
  
  network <- compile $ do
    eKeyPush <- keyboardPress
    
    eResize <- windowResize
    eClose <- windowClose
    
    t <- timerDS 10 100
    
    let
        isStateEvent :: UpdateEvent -> Bool
        isStateEvent (UpdateDisplay _) = False
        isStateEvent (UpdateState _) = True
      
        eSpace  = filterE (== GLFW.CharKey ' ' ) eKeyPush
        eEsc    = filterE (== GLFW.KeyEsc ) eKeyPush
        ePlus   = filterE (== GLFW.CharKey '=' ) eKeyPush
        eMinus  = filterE (== GLFW.CharKey '-' ) eKeyPush
        eStateUpdate = filterE isStateEvent t
        eDispayUpdate = filterE (not . isStateEvent) t
        
        state :: Behavior MyndState
        state = stepper (MyndState 0 0 (0,0)) $ 
                  ((processState <$> state) <@> eStateUpdate)
                  `union` ((clockwise <$> state) <@ ePlus)
                  `union` ((cclockwise <$> state) <@ eMinus)
        
        processState :: MyndState -> UpdateEvent -> MyndState
        processState a (UpdateState b) = a {angle = angle a + (delta a)} 
        
        clockwise :: MyndState -> MyndState
        clockwise a = a {delta = delta a + 0.5}
        
        cclockwise :: MyndState -> MyndState
        cclockwise a = a {delta = delta a - 0.5}
        
        redraw :: MyndState -> UpdateEvent -> IO ()
        redraw b (UpdateDisplay a) = display ((angle b) + (realToFrac a)) >> GLFW.swapBuffers
    
    reactimate $ reshape <$> eResize
    reactimate $ shutdown <$ eEsc
    reactimate $ shutdown <$ eClose
    reactimate $ (redraw <$> state) <@> eDispayUpdate

  actuate network
  
  forever $ threadDelay 1000000

