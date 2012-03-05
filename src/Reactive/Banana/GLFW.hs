module Reactive.Banana.GLFW where

import Reactive.Banana
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent ( threadDelay,forkIO )
import Control.Monad ( forever )

keyCallback :: (GLFW.Key -> IO ()) -> (GLFW.Key -> IO ()) -> GLFW.KeyCallback
keyCallback fp fr key push = 
  if push 
    then fp key
    else fr key

registerKeyboardPush :: NetworkDescription (Event GLFW.Key)
registerKeyboardPush = do
  (addHandlerPush, firePush) <- liftIO newAddHandler
  liftIO $ GLFW.setKeyCallback $ keyCallback firePush (\_ -> return ())
  fromAddHandler addHandlerPush
  
registerKeyboardRelease :: NetworkDescription (Event GLFW.Key)
registerKeyboardRelease = do
  (addHandlerRelease, fireRelease) <- liftIO newAddHandler
  liftIO $ GLFW.setKeyCallback $ keyCallback (\_ -> return ()) fireRelease
  fromAddHandler addHandlerRelease

timer msec = do
  (addHandler, runHandlers) <- liftIO newAddHandler
  liftIO $ forkIO $ forever $ threadDelay (msec * 10 ^ 3) >> liftIO (runHandlers ())
  fromAddHandler addHandler