module Main where

import Control.Monad ( forever,void,when,liftM2 )
import Control.Concurrent ( threadDelay )
import Data.Time
import Data.Interpolable
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.FTGL as FTGL
import Reactive.Banana
import Reactive.Banana.GLFW
import Display

data MyndState
  = MyndState 
  { angle    :: Interpolable GLfloat
  , delta    :: GLfloat
  , position :: (GLfloat,GLfloat)
  }

-- |'main' runs the main program
main :: IO ()
main = do
  initGLFW 800 600
  curry reshape 800 600
  font <- FTGL.createTextureFont "res/DroidSansMono.ttf"
  network <- compile $ do
    eKeyPush <- keyboardPress
    eResize <- windowResize
    eClose <- windowClose

    t <- timerDS 10 200

    let
        isStateEvent :: UpdateEvent -> Bool
        isStateEvent (UpdateDisplay _) = False
        isStateEvent (UpdateState _) = True

        eSpace  = filterE (== key ' ' ) eKeyPush
        eEsc    = filterE (== keyEsc ) eKeyPush
        ePlus   = filterE (== key '=' ) eKeyPush
        eMinus  = filterE (== key '-' ) eKeyPush
        eStateUpdate  = filterE isStateEvent t
        eDispayUpdate = filterE (not . isStateEvent) t

        state :: Behavior MyndState
        state = stepper (MyndState (Interpolable (0::GLfloat) 0 0) 0 (0,0)) $ 
                  ((processState <$> state) <@> eStateUpdate)
                  `union` ((clockwise <$> state) <@ ePlus)
                  `union` ((cclockwise <$> state) <@ eMinus)

        processState :: MyndState -> UpdateEvent -> MyndState
        processState a (UpdateState b) = a {angle = plus (angle a) ((delta a) * (realToFrac b) / 1000)} 

        clockwise :: MyndState -> MyndState
        clockwise a = a {delta = delta a + 0.1}

        cclockwise :: MyndState -> MyndState
        cclockwise a = a {delta = delta a - 0.1}

        redraw :: MyndState -> UpdateEvent -> IO ()
        redraw b (UpdateDisplay a) = display font (interpolate(angle b) (realToFrac a)) >> screenDone

    reactimate $ reshape  <$> eResize
    reactimate $ shutdown <$  eEsc
    reactimate $ shutdown <$  eClose
    reactimate $ (redraw  <$> state) <@> eDispayUpdate

  actuate network

  forever $ threadDelay 1000000
