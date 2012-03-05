module Bindings (display,reshape) where

import Display
{-
keyboard :: IORef GLfloat -> IORef (GLfloat,GLfloat) -> GLFW.KeyCallback
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
-}
