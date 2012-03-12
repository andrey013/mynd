module Main where

import Foreign
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.C.String

import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import Graphics.Rendering.FreeType.Internal.Bitmap
import Graphics.Rendering.FreeType.Internal.Vector
import Graphics.Rendering.FreeType.Internal.Matrix

import Control.Monad ( forever,void,when,liftM2,liftM )
import Control.Concurrent ( threadDelay )
import Data.Maybe
import Data.Time
import Data.Interpolable
import Data.Vector.Storable(unsafeWith)
import Graphics.Rendering.OpenGL
import Reactive.Banana
import Reactive.Banana.GLFW
import Display
import MyndState
import Codec.Picture

makeFont :: IO (Maybe TextureObject)
makeFont = do
  let h = 480
      a = (25 / 360) * pi * 2
  pen    <- mallocForeignPtr
  withForeignPtr pen $ \p -> poke p (FT_Vector
        { x = 300 * 64
        , y = (h - 200) * 64
        })
  matrix <- mallocForeignPtr
  withForeignPtr matrix $ \p-> poke p (FT_Matrix
        { xx = round $   cos a * 0x10000
        , xy = round $ -(sin a * 0x10000)
        , yx = round $   sin a * 0x10000
        , yy = round $   cos a * 0x10000
        })

  libraryptr <- malloc
  library <- do
    print $ libraryptr == nullPtr
    ft_Init_FreeType libraryptr >>= print
    peek libraryptr

  faceptr <- malloc
  print $ faceptr == nullPtr
  face <- withCString "./res/DroidSansMono.ttf" $ \str -> do
    print =<< ft_New_Face library str 0 faceptr
    peek faceptr

  print =<< ft_Set_Char_Size face (50*512) 0 72 0

  withForeignPtr matrix $ \mp ->
    withForeignPtr pen $ \pp -> do
      ft_Set_Transform face mp pp
      slot <- peek $ glyph face

      print =<< ft_Load_Char face
                         (fromIntegral . fromEnum $ 'A')
                         ft_LOAD_RENDER

      numFaces <- peek $ num_faces face
      putStrLn $ "face->num_faces = " ++ show numFaces
      v <- peek $ advance slot
      putStrLn "advance: "
      print v
      numGlyphs <- peek $ num_glyphs face
      putStrLn $ "numGlyphs = " ++ show numGlyphs
      pen' <- peek pp
      poke pp $ FT_Vector { x = x v + x pen'
                          , y = y v + y pen' }
      (FT_Bitmap height width _ pixels _ _ _ _) <- peek $ GS.bitmap slot
      left <- peek $ bitmap_left slot
      top  <- peek $ bitmap_top slot
      putStrLn $ "dim = " ++ show height ++ " x " ++ show width
      putStrLn $ "off = " ++ show top ++ " x " ++ show left
      exts <- get glExtensions
      texture <- if "GL_EXT_texture_object" `elem` exts
                    then liftM listToMaybe $ genObjectNames 1
                    else return Nothing
      textureBinding Texture2D $= texture

      textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
      textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
      textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
      texImage2D Nothing NoProxy 0 RGBA'
                              (TextureSize2D (fromIntegral $ width) (fromIntegral $ height))
                              0 (PixelData Alpha UnsignedByte pixels)
      return texture

makeTexture :: String -> IO (Maybe TextureObject)
makeTexture filename = do
  (Right qwe) <- readImage filename
  (Right (ImageRGBA8 (Image width height pixels))) <- readImage filename

  exts <- get glExtensions
  texture <- if "GL_EXT_texture_object" `elem` exts
                 then liftM listToMaybe $ genObjectNames 1
                 else return Nothing
  textureBinding Texture2D $= texture

  textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
  unsafeWith pixels $ texImage2D Nothing NoProxy 0 RGBA'
                          (TextureSize2D (fromIntegral width) (fromIntegral height))
                          0 . PixelData RGBA UnsignedByte
  return texture

-- |'main' runs the main program
main :: IO ()
main = do
  initGLFW 800 600
  curry reshape 800 600
  --tex <- makeTexture "res/pixelTest.png"
  tex <- makeFont
  network <- compile $ do
    eKeyPush <- keyboardPress
    eResize <- windowResize
    eClose <- windowClose

    t <- timerDS 10 200

    let
        isStateEvent :: UpdateEvent -> Bool
        isStateEvent (UpdateDisplay _) = False
        isStateEvent (UpdateState _) = True

        eSpace  = filterE (== key ' ' ) eKeyPush
        eEsc    = filterE (== keyEsc ) eKeyPush
        ePlus   = filterE (== key '=' ) eKeyPush
        eMinus  = filterE (== key '-' ) eKeyPush
        eStateUpdate  = filterE isStateEvent t
        eDispayUpdate = filterE (not . isStateEvent) t

        state :: Behavior MyndState
        state = stepper (MyndState (Interpolable (0::GLfloat) 0 0) 0 (0,0)) $ 
                  ((processState <$> state) <@> eStateUpdate)
                  `union` ((clockwise <$> state) <@ ePlus)
                  `union` ((cclockwise <$> state) <@ eMinus)

        processState :: MyndState -> UpdateEvent -> MyndState
        processState a (UpdateState b) = a {angle = plus (angle a) (delta a * realToFrac b / 1000)}

        clockwise :: MyndState -> MyndState
        clockwise a = a {delta = delta a + 0.1}

        cclockwise :: MyndState -> MyndState
        cclockwise a = a {delta = delta a - 0.1}

        redraw :: MyndState -> UpdateEvent -> IO ()
        redraw b (UpdateDisplay a) = display tex (interpolate(angle b) (realToFrac a)) >> screenDone

    --reactimate $ (\a -> putStrLn $ show a) <$> eKeyPush
    reactimate $ reshape  <$> eResize
    reactimate $ shutdown <$  eEsc
    reactimate $ shutdown <$  eClose
    reactimate $ (redraw  <$> state) <@> eDispayUpdate

  actuate network

  forever $ threadDelay 1000000
