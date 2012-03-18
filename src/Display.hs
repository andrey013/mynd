module Display (display,reshape) where

import Graphics.Rendering.OpenGL
import Util.Util
import MyndState
import MyndNet

calcWidth (MyndNode _ _ w []) = w + 50
calcWidth (MyndNode _ _ w c@(cx:cxs)) = max (sum $ map calcWidth c) w

display :: MyndState -> IO ()
display state1 = do
  clearColor $= Color4 1 1 1 1
  clear [ColorBuffer, DepthBuffer]
  texture Texture2D $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  display' state1 (-250) (250)
  texture Texture2D $= Disabled
  blend $= Disabled
  where
    display' state@(MyndState {net=node@(MyndNode _ _ w [])}) x y =
      display'' state (x + (fromIntegral $ calcWidth node - w - 50) / 2) y
    display' state@(MyndState {delta=angle, net=node@(MyndNode _ _ w c@(cx:cxs))}) x y = do
      display'' state (x + (fromIntegral $ calcWidth node - w - 50) / 2) y
      display' (state{net=cx}) x (y-100)
      mapM_ (\(a, b) -> display' (state{net=b}) (x + (fromIntegral a)) (y-100))
        $ zip ((calcWidth cx):(zipWith (+) (map calcWidth c) (map calcWidth cxs))) cxs
    display'' state@(MyndState {delta=angle, net=(MyndNode _ tex _ _)}) x y =
      preservingMatrix $ do
        translate $ Vector3 x y (angle::GLfloat)
        textureBinding Texture2D $= tex
        color $ Color3 (1::GLfloat) 1 1
        plane (256::GLfloat)


reshape :: (Int, Int) -> IO ()
reshape (w,     0)      = reshape (w, 1) -- prevent divide by zero
reshape (w,     h)      = do
  viewport $= (Position 0 0, Size width height)
  matrixMode $= Projection
  loadIdentity
  matrixPerspectivePixelPerfect width height 2000 10 (-10)
  matrixMode $= Modelview 0
  loadIdentity
  flush
  where width  = (2::GLsizei) * fromIntegral (w `div` 2)
        height = (2::GLsizei) * fromIntegral (h `div` 2)

-- |width and height defines the 2D space available at z=0, must be the same
-- as the size of the viewport.
-- z_near defines the z position of the near plane, must be greater than 0.
-- z_far defines the z position of the far plane, must be lesser than 0.
-- z_eye defines the position of the viewer, must be greater that z_near.
matrixPerspectivePixelPerfect :: GLsizei -> GLsizei -> GLfloat -> GLfloat -> GLfloat -> IO ()
matrixPerspectivePixelPerfect w h z_eye z_near z_far = do 
  m <- newMatrix RowMajor [(2 * z_eye) / width,                    0,                 0,     0
                          ,                  0, (2 * z_eye) / height,                 0,     0
                          ,                  0,                    0, ktz - ksz * z_eye,    -1
                          ,       0 :: GLfloat,                    0,               ksz, z_eye]
  (matrix (Just Projection) :: StateVar(GLmatrix GLfloat)) $= m
  where kdn = z_eye - z_near
        kdf = z_eye - z_far
        ksz = - (kdf + kdn) / (kdf - kdn)
        ktz = - (2 * kdn * kdf) / (kdf - kdn)
        width  = fromIntegral w
        height = fromIntegral h
