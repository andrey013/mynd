module Graphics.Rendering.Font where

import Foreign
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types

import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import Graphics.Rendering.FreeType.Internal.Bitmap
import Graphics.Rendering.FreeType.Internal.Vector
import Graphics.Rendering.FreeType.Internal.Matrix

import Graphics.Rendering.OpenGL

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as M

import Data.Maybe
import Control.Monad ( forever,void,when,liftM2,liftM,forM )

type Font = FT_Face

makeFont :: String -> IO Font
makeFont filename = do

  libraryptr <- malloc
  library <- do
    ft_Init_FreeType libraryptr
    peek libraryptr

  faceptr <- malloc
  face <- withCString filename $ \str -> do
    ft_New_Face library str 0 faceptr
    peek faceptr
  return face

renderText :: Font -> Int -> Int -> String -> IO (Maybe TextureObject)
renderText face size lineHeight string = do

  ft_Set_Pixel_Sizes face 0 $ fromIntegral size
  pen    <- mallocForeignPtr
  withForeignPtr pen $ \p -> poke p (FT_Vector
        { x = 0
        , y = 0
        })

  withForeignPtr pen $ \pp -> do
    let image = S.replicate (128 * 128) (0 :: Word8)
        buf = S.create $ do
            vec <- S.thaw image
            sequence $ map (\(a, b) -> M.write vec a b) $ unsafePerformIO $ renderText' pp string
            return vec
    exts <- get glExtensions
    texture <- if "GL_EXT_texture_object" `elem` exts
                  then liftM listToMaybe $ genObjectNames 1
                  else return Nothing
    textureBinding Texture2D $= texture

    textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
    textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
    textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
    S.unsafeWith buf $ texImage2D Nothing NoProxy 0 RGBA'
                                  (TextureSize2D (fromIntegral $ 128) (fromIntegral $ 128))
                                  0 . PixelData Alpha UnsignedByte
    return texture
  where
    renderText' _ [] = return []
    renderText' pen (c:xc) = do
      ft_Set_Transform face nullPtr pen
      slot <- peek $ glyph face

      ft_Load_Char face (fromIntegral . fromEnum $ c)
                        ft_LOAD_RENDER

      v <- peek $ advance slot
      pen' <- peek pen
      poke pen $ FT_Vector { x = x v + x pen'
                          , y = y v + y pen' }

      (FT_Bitmap height width _ pixels _ _ _ _) <- peek $ GS.bitmap slot
      left <- liftM fromIntegral $ peek $ bitmap_left slot
      top  <- liftM fromIntegral $ peek $ bitmap_top slot

      let xMax = left + fromIntegral width
          yMax = lineHeight - top + fromIntegral height
      return $ concat (map (\(i,p) -> map (\(j,q) ->
                  let index = q * width + p
                      imageIndex = fromIntegral $ j * 128 + i
                      b = unsafePerformIO $ peek $ pixels `plusPtr` fromIntegral index
                  in if b>0 then (imageIndex, b) else (0,0)) (zip [ lineHeight - top .. yMax - 1] [0 .. ])) (zip [ left .. xMax - 1] [0 .. ])) ++ (unsafePerformIO $ renderText' pen xc)
