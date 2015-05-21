module Asteroids.Utils where

import Asteroids.Types
import Linear.V2
import Linear.Matrix

vectorLength :: (V2 Float) -> Float
vectorLength (V2 x y) = sqrt $ x**2 + y**2

mulVector :: (V2 Float) -> Float -> (V2 Float)
mulVector (V2 x y) k = V2 (k*x) (k*y) 

distance :: (Physical a, Physical b) => a -> b -> Float
distance x y = vectorLength $ pos x - pos y

rotationMatrix ::  Float -> V2 (V2 Float) --матрица поворота, угол в радианах
rotationMatrix a =  V2 (V2 (cos a) (- sin a)) (V2 (sin a) (cos a))

rotateSpeed::Float -> Float -> V2 Float
rotateSpeed a v = (rotationMatrix a) !* (V2 v 0) 

vX :: (V2 a) -> a
vX (V2 x y) = x

vY :: (V2 a) -> a
vY (V2 x y) = y
