module Data.Interpolable where

data Interpolable a 
  = Interpolable
  {
    oldValue :: a
   ,value :: a
   ,oldSpeed :: a
  }

writeVal :: (Num a) => Interpolable a -> a -> Interpolable a
writeVal (Interpolable ov val os ) newVal = Interpolable val newVal (val - ov)

interpolate :: (Num a) => Interpolable a -> a -> a
interpolate (Interpolable ov val os) degree = ov + speed * degree
  where speed = val - ov
        
emptyInterpolable = Interpolable 0 0 0

plus :: (Num a) => Interpolable a -> a -> Interpolable a
plus i@(Interpolable ov val os) v = writeVal i (val + v)
