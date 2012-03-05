module Reactive.Banana.GLFW where

import Reactive.Banana
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent ( threadDelay,forkIO )
import Control.Monad ( forever )
import System.Exit ( exitSuccess )

keyCallback :: (GLFW.Key -> IO ()) -> (GLFW.Key -> IO ()) -> GLFW.KeyCallback
keyCallback fp _  key True  = fp key
keyCallback _  fr key False = fr key

keyboardPress :: NetworkDescription (Event GLFW.Key)
keyboardPress = do
  (addHandlerPress, firePress) <- liftIO newAddHandler
  liftIO $ GLFW.setKeyCallback $ keyCallback firePress (\_ -> return ())
  fromAddHandler addHandlerPress
  
keyboardRelease :: NetworkDescription (Event GLFW.Key)
keyboardRelease = do
  (addHandlerRelease, fireRelease) <- liftIO newAddHandler
  liftIO $ GLFW.setKeyCallback $ keyCallback (\_ -> return ()) fireRelease
  fromAddHandler addHandlerRelease

windowResize :: NetworkDescription (Event (Int, Int))
windowResize = do
  (addHandler, fire) <- liftIO newAddHandler
  liftIO $ GLFW.setWindowSizeCallback $ (\w h -> liftIO (fire (w, h)))
  fromAddHandler addHandler

windowClose :: NetworkDescription (Event ())
windowClose = do
  (addHandler, fire) <- liftIO newAddHandler
  liftIO $ GLFW.setWindowCloseCallback $ liftIO (fire () >> return False)
  fromAddHandler addHandler

timer :: Int -> NetworkDescription (Event ())
timer msec = do
  (addHandler, fire) <- liftIO newAddHandler
  liftIO $ forkIO $ forever $ threadDelay (msec * 10 ^ 3) >> liftIO (fire ())
  fromAddHandler addHandler
  
shutdown :: IO ()
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  exitSuccess