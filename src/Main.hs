module Main where

import Control.Monad ( forever,void,when,liftM2,liftM,forM )
import Control.Concurrent ( threadDelay )
import Data.Maybe
import Data.Time
import Data.Interpolable
import Data.Vector.Storable( unsafeWith )
import Codec.Picture

import Graphics.Rendering.OpenGL
import Graphics.Rendering.Font
import Reactive.Banana
import Reactive.Banana.GLFW
import Display
import MyndState
import MyndNet


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
  font <- makeFont "res/ttf/DejaVuSans.ttf"
  tex  <- renderText font 256 128 6 "Корень"
  tex1 <- renderText font 256 128 6 "Лист"
  tex2 <- renderText font 256 128 6 "Листок"
  tex3 <- renderText font 256 128 6 "Л"
  network <- compile $ do
    eKeyPush <- keyboardPress
    eResize <- windowResize
    eClose <- windowClose

    t <- timerDS 20 200

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
        state = stepper (MyndState (Interpolable (0::GLfloat) 0 0) 0 (0,0)
                                   (MyndNode "Корень" 200 tex
                                    [ MyndNode "Корень" 160 tex1 []
                                    , MyndNode "Корень" 150 tex2
                                       [ MyndNode "Корень" 50 tex3 []
                                       , MyndNode "Корень" 50 tex3 []
                                       ]
                                    ]
                                   )
                        ) $
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
        redraw b (UpdateDisplay a) = display b >> screenDone

    --reactimate $ (\a -> putStrLn $ show a) <$> eKeyPush
    reactimate $ reshape  <$> eResize
    reactimate $ shutdown <$  eEsc
    reactimate $ shutdown <$  eClose
    reactimate $ (redraw  <$> state) <@> eDispayUpdate

  actuate network

  forever $ threadDelay 1000000
