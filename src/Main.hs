
import Graphics.Gloss.Interface.IO.Game
import Linear.V2
import Linear.Matrix
import Data.Monoid (mconcat)
data Object = Player (V2 Float) Float Float|Bullet (V2 Float) (V2 Float)

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

rotateSpeed::Float -> Float -> V2 Float
rotateSpeed a v = (rotationMatrix a) !* (V2 v 0) 
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
spaceToPic:: Space -> [Picture]
spaceToPic [] = []
spaceToPic ((Bullet (V2 x y) _):space) = (Color white $ translate x y $ Circle 5):(spaceToPic space)

drawSpace :: Space -> IO Picture --отрисовывает все объекты, используя их коордианты
drawSpace ((Player (V2 x y) v a):space) = do
   return $ mconcat $ (Color white $ translate x y $ rotate ((-180)*a/pi) $ polygon [(0, 10), (0, -10), (20, 0)]):(spaceToPic space)

--------------------------------------------------------------------------------
handleInput :: Event -> Space -> IO Space -- w - придание ускорения кораблю, d - поворот по часовой, a - против
handleInput (EventKey (Char 'w') Down _ _) ((Player (V2 x y) v a):rest) = return $ (Player (V2 x y) (v+accel) a):rest
handleInput (EventKey (Char 'a') Down _ _) ((Player (V2 x y) v a):rest) = return $ (Player (V2 x y) v (a+rotSpeed)):rest
handleInput (EventKey (Char 'd') Down _ _) ((Player (V2 x y) v a):rest) = return $ (Player (V2 x y) v (a-rotSpeed)):rest
handleInput (EventKey (SpecialKey KeySpace) Down _ _) ((Player (V2 x y) v a):rest) = return $ (Player (V2 x y) v a):rest ++ [Bullet (V2 (x+3) (y+1)) (rotateSpeed a 5)] 
handleInput  _ space = return space

move::Space->Space
move [] = []
move ((Bullet (V2 x y) (V2 vx vy)):space) = (Bullet (V2 (x+vx) (y+vy)) (V2 vx vy)):(move space)

stepGame :: Float -> Space -> IO Space
stepGame _ ((Player (V2 x y) v a):space) = do
   print $ length space
   let V2 vx vy = (rotationMatrix a) !* (V2 v 0) --поворот вектора скорости
   return $ (Player (V2 (x'+vx) (y'+vy))  (max (v-decel) 0) a):(move space) where --проверка на выход за пределы экрана
      x'  | x > 250 = -249
          | x < -250 = 249
          | otherwise = x
      y'  | y > 250 = -249
          | y < -250 = 249	
          | otherwise = y 
stepGame _ space = return space
