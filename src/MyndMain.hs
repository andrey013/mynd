module Main where

import Control.Monad ( forever,void,when,liftM2 )
import Control.Concurrent ( threadDelay )
import Data.Time
import Data.Interpolable
import Graphics.Rendering.OpenGL as GL
import Bindings
import Reactive.Banana
import Reactive.Banana.GLFW

data MyndState
  = MyndState 
  {
    angle :: Interpolable GLfloat
   ,delta :: GLfloat
   ,position :: (GLfloat,GLfloat)
   --,myndTime :: MyndTime
  }
    
-- |'main' runs the main program
main :: IO ()
main = do
  initGLFW
  
  network <- compile $ do
    eKeyPush <- keyboardPress
    
    eResize <- windowResize
    eClose <- windowClose
    
    t <- timerDS 1 100
    
    let
        isStateEvent :: UpdateEvent -> Bool
        isStateEvent (UpdateDisplay _) = False
        isStateEvent (UpdateState _) = True
      
        eSpace  = filterE (== key ' ' ) eKeyPush
        eEsc    = filterE (== keyEsc ) eKeyPush
        ePlus   = filterE (== key '=' ) eKeyPush
        eMinus  = filterE (== key '-' ) eKeyPush
        eStateUpdate = filterE isStateEvent t
        eDispayUpdate = filterE (not . isStateEvent) t
        
        state :: Behavior MyndState
        state = stepper (MyndState (Interpolable (0::GLfloat) 0 0) 0 (0,0)) $ 
                  ((processState <$> state) <@> eStateUpdate)
                  `union` ((clockwise <$> state) <@ ePlus)
                  `union` ((cclockwise <$> state) <@ eMinus)
        
        processState :: MyndState -> UpdateEvent -> MyndState
        processState a (UpdateState b) = a {angle = plus (angle a) ((delta a) * (realToFrac b) / 1000)} 
        
        clockwise :: MyndState -> MyndState
        clockwise a = a {delta = delta a + 50}
        
        cclockwise :: MyndState -> MyndState
        cclockwise a = a {delta = delta a - 50}
        
        redraw :: MyndState -> UpdateEvent -> IO ()
        redraw b (UpdateDisplay a) = display (interpolate(angle b) (realToFrac a)) >> screenDone
    
    reactimate $ reshape <$> eResize
    reactimate $ shutdown <$ eEsc
    reactimate $ shutdown <$ eClose
    reactimate $ (redraw <$> state) <@> eDispayUpdate

  actuate network
  
  forever $ threadDelay 1000000

