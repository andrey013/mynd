module Util.Util where

import Graphics.Rendering.OpenGL

-- |'fib' returns given Fibonacci number
fib :: Int -> Int
fib = (!!) fibs

-- |'fibs' is a list of Fibonacci numbers
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

cube w = 
  renderPrimitive Quads $ do
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) w (-w)

plane width = renderPrimitive Quads $ do
  let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
      vertex3f = vertex :: Vertex3 GLfloat -> IO ()
      w = width / 2
  texCoord2f $ TexCoord2 0 1
  vertex3f   $ Vertex3 (-w) (-w) 0
  texCoord2f $ TexCoord2 1 1
  vertex3f   $ Vertex3 w (-w) 0
  texCoord2f $ TexCoord2 1 0
  vertex3f   $ Vertex3   w w 0
  texCoord2f $ TexCoord2 0 0
  vertex3f   $ Vertex3   (-w) w 0

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n' = let n = fromIntegral n' in map (\k -> let t = 2*pi*k/n in (sin t, cos t, 0.0))  [1..n]
