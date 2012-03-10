module Display (display,reshape) where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.FTGL as FTGL
import Util.Util

display ::FTGL.Font -> GLfloat -> IO ()
display font angle = do --(x,y) = do
  clear [ColorBuffer, DepthBuffer]
  preservingMatrix $ do
    --texture Texture2D $= Disabled
    --texture TextureCubeMap $= Disabled
    --rotate angle $ Vector3 0 0 (1::GLfloat)
    --scale (4::GLfloat) (4::GLfloat) (0::GLfloat)
    mapM_ (\(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
      --translate $ Vector3 x y z
      FTGL.setFontFaceSize font 9 72
      FTGL.renderFont font "ЙЦУКЕН ыапдлоывапЛОЛавовыд" FTGL.Front
      --cube (0.1::GLfloat)
      ) $ points 1
    --texture Texture2D $= Disabled
{-
idle angle delta = do
  a <- readIORef angle
  d <- readIORef delta
  writeIORef angle $ a + d
  -}
reshape :: (Int, Int) -> IO ()
reshape (w,     0)      = reshape (w, 1) -- prevent divide by zero
reshape (w,     h)      = do
  textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  
  viewport $= (Position 0 0, Size (fromIntegral $ 2 * (w `div` 2)) (fromIntegral $ 2 * (h `div` 2)))
  matrixMode $= Projection
  loadIdentity
  --perspective 45 (fromIntegral w/fromIntegral h) 0.1 100
  ortho2D (fromIntegral $ -w `div` 2) (fromIntegral $ w `div` 2)
    (fromIntegral $ -h `div` 2) (fromIntegral $ h `div` 2)
  matrixMode $= Modelview 0
  loadIdentity
  --translate $ Vector3 0.375 0.375 (0::GLfloat)
  flush