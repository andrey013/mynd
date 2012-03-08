module Reactive.Banana.GLFW where

import Reactive.Banana
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent ( threadDelay,forkIO )
import Control.Monad ( forever,when )
import System.Exit ( exitSuccess )
import Data.Time
import Data.IORef

data UpdateEvent
  = UpdateState Float
  | UpdateDisplay Float

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
  liftIO $ GLFW.setWindowSizeCallback $ \w h -> liftIO $ fire (w, h)
  fromAddHandler addHandler

windowClose :: NetworkDescription (Event ())
windowClose = do
  (addHandler, fire) <- liftIO newAddHandler
  liftIO $ GLFW.setWindowCloseCallback $ liftIO (fire () >> return False)
  fromAddHandler addHandler

timerDS :: Int -> Int -> NetworkDescription (Event UpdateEvent)
timerDS d s = do
  (addHandler, fire) <- liftIO newAddHandler
  liftIO $ forkIO $ do
    t <- getCurrentTime
    r <- newIORef t
    forever $ do
      threadDelay (d * 10 ^ 3)
      oldTime <- readIORef r
      newTime <- getCurrentTime
      let diff = fromEnum $ (diffUTCTime newTime oldTime) / 10 ^ 9
      if ( diff > s ) 
        then do
          let newOldTime = addUTCTime ((fromIntegral s) / 10 ^ 3) oldTime
          writeIORef r newOldTime
          fire $ UpdateState $ fromIntegral s
          fire $ UpdateDisplay $ (fromIntegral . fromEnum) (diffUTCTime newTime newOldTime) / ((fromIntegral s) * 10 ^ 9)
        else
          fire $ UpdateDisplay $ (fromIntegral . fromEnum) (diffUTCTime newTime oldTime) / ((fromIntegral s) * 10 ^ 9)
  fromAddHandler addHandler

shutdown :: IO ()
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  exitSuccess