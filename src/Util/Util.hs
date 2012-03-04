module Util.Util where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

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

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n' = let n = fromIntegral n' in map (\k -> let t = 2*pi*k/n in (sin(t),cos(t),0.0))  [1..n]
