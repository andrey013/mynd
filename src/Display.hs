module Display (display,reshape) where

import Graphics.Rendering.OpenGL as GL
import Util.Util

display :: GLfloat -> IO ()
display angle = do --(x,y) = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  --(x,y) <- readIORef position
  --translate $ Vector3 x y 0
  preservingMatrix $ do
    --a <- readIORef angle
    --let a = angle
    rotate angle $ Vector3 0 0 (1::GLfloat)
    scale (0.7::GLfloat) (0.7::GLfloat) (0.7::GLfloat)
    mapM_ (\(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
      translate $ Vector3 x y z
      cube (0.1::GLfloat)
      ) $ points 20
{-
idle angle delta = do
  a <- readIORef angle
  d <- readIORef delta
  writeIORef angle $ a + d
  -}
reshape :: (Int, Int) -> IO ()
reshape (w,     0)      = reshape (w, 1) -- prevent divide by zero
reshape (w,     h)      = 
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  --matrixMode $= Projection
  --loadIdentity
  --perspective 45 (fromIntegral w/fromIntegral h) 0.1 100
  --matrixMode $= Modelview 0
  --loadIdentity
  --flush