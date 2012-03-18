module MyndNet where

import Graphics.Rendering.OpenGL

data MyndNode
  = MyndNode
  { title :: String
  , tex   :: Maybe TextureObject
  , children :: [MyndNode]
  }
