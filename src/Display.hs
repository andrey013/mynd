module Display (display,idle) where

import Data.IORef
import Graphics.Rendering.OpenGL as GL
import Util.Util

display angle position = do 
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  (x,y) <- readIORef position
  translate $ Vector3 x y 0
  preservingMatrix $ do
    a <- readIORef angle
    rotate a $ Vector3 0 0 (1::GLfloat)
    scale (0.7::GLfloat) (0.7::GLfloat) (0.7::GLfloat)
    mapM_ (\(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
      translate $ Vector3 x y z
      cube (0.1::GLfloat)
      ) $ points 20

idle angle delta = do
  a <- readIORef angle
  d <- readIORef delta
  writeIORef angle $ a + d