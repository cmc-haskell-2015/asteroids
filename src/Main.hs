
import Graphics.Gloss.Interface.IO.Game
import Linear.V2
import Linear.Matrix

data Object = Player (V2 Float) Float Float

type Space = [Object]

initialSpace :: Space -- изначальное состояние, в нем только игрок
initialSpace = [Player (V2 0 0) 0 0]

rotationMatrix ::  Float -> V2 (V2 Float) --матрица поворота, угол в радианах
rotationMatrix a =  V2 (V2 (cos a) (- sin a)) (V2 (sin a) (cos a))

accel::Float --ускорение, прибавляется к скорости при нажатии d (скорость - пиксели*масштаб в секунду)
accel = 2

decel::Float --замедление, естественная убыль скорости
decel = 0.05

rotSpeed::Float --скорость поворота, радианы за нажатие
rotSpeed = pi/8
--------------------------------------------------------------------------------
main :: IO () --входная точка
main = do

  playIO
    (InWindow "Asteroids" (500, 500) (500, 500)) --имя, размеры, место окна
    black --цвет фона
    60 --fps
    initialSpace --начальный мир
    drawSpace --функция прорисовки
    handleInput --обработчик нажатий клавиш
    stepGame --обработчик каждого кадра

--------------------------------------------------------------------------------
drawSpace :: Space -> IO Picture --отрисовывает все объекты, используя их коордианты
drawSpace [Player (V2 x y) v a] = do
   return $ Color white   $ translate x y $ polygon [(x, y+10), (x, y-10), (x+20, y)]
   --return $ Color white   $ translate x y $ rotate (180*a/pi) $ polygon [(x, y+10), (x, y-10), (x+20, y)]

--------------------------------------------------------------------------------
handleInput :: Event -> Space -> IO Space -- w - придание ускорения кораблю, d - поворот по часовой, a - против
handleInput (EventKey (Char 'w') Down _ _) ([Player (V2 x y) v a]) = return $ [Player (V2 x y) (v+accel) a]
handleInput (EventKey (Char 'a') Down _ _) ([Player (V2 x y) v a]) = return $ [Player (V2 x y) v (a+rotSpeed)]
handleInput (EventKey (Char 'd') Down _ _) ([Player (V2 x y) v a]) = return $ [Player (V2 x y) v (a-rotSpeed)]
handleInput  _ space = return space



stepGame :: Float -> Space -> IO Space
stepGame _ ([Player (V2 x y) v a]) = do
   let V2 vx vy = (rotationMatrix a) !* (V2 v 0) --поворот вектора скорости
   return $ [Player (V2 (x'+vx) (y'+vy))  (max (v-decel) 0) a] where --проверка на выход за пределы экрана
      x'  | x > 150 = -149
          | x < -150 = 149
          | otherwise = x
      y'  | y > 150 = -149
          | y < -150 = 149
          | otherwise = y 
stepGame _ space = return space
