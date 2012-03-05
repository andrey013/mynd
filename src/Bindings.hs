module Bindings (idle,display,reshape,keyboard,shutdown) where

import Data.IORef
import Control.Monad ( void )
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.Exit ( exitSuccess )
import Display

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitSuccess
  return True

keyboard :: IORef GLfloat -> IORef (GLfloat,GLfloat) -> GLFW.KeyCallback
keyboard _ _ GLFW.KeyEsc    True = void shutdown
keyboard d _ (GLFW.CharKey ' ')  True = do
  a <- readIORef d
  writeIORef d $ -a
keyboard _ p GLFW.KeyLeft   True = do
  (x,y) <- readIORef p
  writeIORef p (x-0.1,y)
keyboard _ p GLFW.KeyRight  True = do
  (x,y) <- readIORef p
  writeIORef p (x+0.1,y)
keyboard _ p GLFW.KeyUp     True = do
  (x,y) <- readIORef p
  writeIORef p (x,y+0.1)
keyboard _ p GLFW.KeyDown   True = do
  (x,y) <- readIORef p
  writeIORef p (x,y-0.1::GLfloat)
keyboard _ _ _           _    = return ()

reshape :: GLFW.WindowSizeCallback
reshape w     0      = reshape w 1 -- prevent divide by zero
reshape w     h      = 
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  --matrixMode $= Projection
  --loadIdentity
  --perspective 45 (fromIntegral w/fromIntegral h) 0.1 100
  --matrixMode $= Modelview 0
  --loadIdentity
  --flush

