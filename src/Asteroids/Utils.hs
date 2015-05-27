-- | ???
module Asteroids.Utils where

import Asteroids.Types
import Linear.V2
import Linear.Matrix

-- | Длина вектора.
vectorLength :: (V2 Float) -> Float
vectorLength (V2 x y) = sqrt $ x**2 + y**2

-- | Умножение вектора на скаляр.
mulVector :: (V2 Float) -> Float -> (V2 Float)
mulVector (V2 x y) k = V2 (k*x) (k*y) 

-- | Расстояние между двумя объектами.
distance :: (Physical a, Physical b) => a -> b -> Float
distance x y = vectorLength $ pos x - pos y

-- | Матрица поворота, угол в радианах.
rotationMatrix ::  Angle -> V2 (V2 Float)
rotationMatrix a =  V2 (V2 (cos a) (- sin a)) (V2 (sin a) (cos a))

-- | Повернуть скорость на угол.
rotateSpeed::Float -> Float -> V2 Float
rotateSpeed a v = (rotationMatrix a) !* (V2 v 0) 

-- | Х-компонента вектора.
vX :: (V2 a) -> a
vX (V2 x _) = x

-- | У-компонента вектора.
vY :: (V2 a) -> a
vY (V2 _ y) = y
