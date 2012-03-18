module MyndNet where

import Graphics.Rendering.OpenGL

data MyndNode
  = MyndNode
  { title :: String
  , width :: Int
  , tex   :: Maybe TextureObject
  , children :: [MyndNode]
  }
