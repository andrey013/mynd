module Bindings where

import Control.Monad ( void )
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import System.Exit ( exitSuccess )

shutdown :: GLFW.WindowCloseCallback
shutdown = do
  GLFW.closeWindow
  GLFW.terminate
  _ <- exitSuccess
  return True

keyboard :: GLFW.KeyCallback
keyboard GLFW.KeyEsc True = void shutdown
keyboard _           _    = return ()

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

