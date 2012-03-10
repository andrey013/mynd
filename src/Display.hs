module Display (display,reshape) where

import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.FTGL as FTGL
import Util.Util

display ::FTGL.Font -> GLfloat -> IO ()
display font angle = do
  clear [ColorBuffer, DepthBuffer]
  preservingMatrix $ do
    --rotate angle $ Vector3 0 0 (1::GLfloat)
    --scale (4::GLfloat) (4::GLfloat) (0::GLfloat)
    mapM_ (\(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
      translate $ Vector3 (0) (0) (angle::GLfloat)
      FTGL.setFontFaceSize font 9 72
      FTGL.renderFont font (show angle) FTGL.Front
      --cube (0.1::GLfloat)
      ) $ points 1

reshape :: (Int, Int) -> IO ()
reshape (w,     0)      = reshape (w, 1) -- prevent divide by zero
reshape (w,     h)      = do
  viewport $= (Position 0 0, Size width height)
  matrixMode $= Projection
  loadIdentity
  matrixPerspectivePixelPerfect width height 100 50 (-50)
  matrixMode $= Modelview 0
  loadIdentity
  flush
  where width  = (2::GLsizei) * (fromIntegral $ w `div` 2)
        height = (2::GLsizei) * (fromIntegral $ h `div` 2)

-- |width and height defines the 2D space available at z=0, must be the same
-- as the size of the viewport.
-- z_near defines the z position of the near plane, must be greater than 0.
-- z_far defines the z position of the far plane, must be lesser than 0.
-- z_eye defines the position of the viewer, must be greater that z_near.
matrixPerspectivePixelPerfect :: GLsizei -> GLsizei -> GLfloat -> GLfloat -> GLfloat -> IO ()
matrixPerspectivePixelPerfect w h z_eye z_near z_far = do 
  m <- (newMatrix RowMajor [(2 * z_eye) / width,                    0,                 0,     0
                           ,                  0, (2 * z_eye) / height,                 0,     0
                           ,                  0,                    0, ktz - ksz * z_eye,    -1
                           ,       0 :: GLfloat,                    0,               ksz, z_eye])
  ((matrix (Just Projection))::StateVar(GLmatrix GLfloat)) $= m
  where kdn = z_eye - z_near
        kdf = z_eye - z_far
        ksz = - (kdf + kdn) / (kdf - kdn)
        ktz = - (2 * kdn * kdf) / (kdf - kdn)
        width  = (fromIntegral w)
        height = (fromIntegral h)
