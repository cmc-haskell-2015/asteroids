
import Graphics.Gloss.Interface.IO.Game
import Linear.V2
import Linear.Matrix
import Data.Monoid (mconcat)
import System.Random
data Object = Player 
   { position  :: (V2 Float)
   , speed   :: Float
   , angle :: Float 
   , time :: Float
   , existence :: Bool
   , jet :: Bool}
   |Bullet (V2 Float) (V2 Float) Bool|Asteroid (V2 Float) (V2 Float) Float Bool deriving (Show) 
type Space = [Object]

initialSpace :: Space -- изначальное состояние, в нем только игрок
initialSpace = [Player (V2 0 0) 0 0 0 True False, 
                Asteroid (V2 100 100) (V2 (-0.1) 0) 4 True,
                Asteroid (V2 (-100) 120) (V2 (-1) 0.5) 3 True,
                Asteroid (V2 50 (-100)) (V2 2 (-1)) 2 True,
                Asteroid (V2 (-200) (-200)) (V2 2 4) 1 True]

rotationMatrix ::  Float -> V2 (V2 Float) --матрица поворота, угол в радианах
rotationMatrix a =  V2 (V2 (cos a) (- sin a)) (V2 (sin a) (cos a))

accel::Float --ускорение, прибавляется к скорости при нажатии d (скорость - пиксели*масштаб в секунду)
accel = 0.1

edgeTop::Float
edgeTop = 250

edgeRight::Float
edgeRight = 250

bulletR::Float
bulletR = 10

asteroidR::Float
asteroidR = 10

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
spaceToPic ((Player (V2 x y) v a _ _ _):space) = (Color white $ translate x y $ rotate ((-180)*a/pi) $ polygon [(0, 10), (0, -10), (20, 0)]):(spaceToPic space)
spaceToPic ((Bullet (V2 x y) _ _):space) = (Color white $ translate x y $ Circle 5):(spaceToPic space)
spaceToPic ((Asteroid (V2 x y) _ s _):space) = (Color white $ translate x y $ ThickCircle (5*s) (10*s)):(spaceToPic space)

drawSpace :: Space -> IO Picture --отрисовывает все объекты, используя их коордианты
drawSpace ((Player (V2 x y) v a _ False _):space) = return $ scale 0.3 0.3 . translate (-400) 0 . color red . text $ "Game Over!"
drawSpace space = return $ mconcat $ spaceToPic space

--------------------------------------------------------------------------------
handleInput :: Event -> Space -> IO Space -- w - придание ускорения кораблю, d - поворот по часовой, a - против
handleInput (EventKey (Char 'w') Down _ _) ((Player (V2 x y) v a t e j):rest) = return $ (Player (V2 x y) (v) a t e True):rest
handleInput (EventKey (Char 'w') Up _ _) ((Player (V2 x y) v a t e j):rest) = return $ (Player (V2 x y) (v) a t e False):rest
handleInput (EventKey (Char 'a') Down _ _) ((Player (V2 x y) v a t e j):rest) = return $ (Player (V2 x y) v (a+rotSpeed) t e j):rest
handleInput (EventKey (Char 'd') Down _ _) ((Player (V2 x y) v a t e j):rest) = return $ (Player (V2 x y) v (a-rotSpeed) t e j):rest
handleInput (EventKey (SpecialKey KeySpace) Down _ _) ((Player (V2 x y) v a t e j):rest) = return $ (Player (V2 x y) v a t e j):rest ++ [Bullet (V2 (x+3) (y+1)) (rotateSpeed a 5) True] 
handleInput  _ space = return space

move::Space->Space
move [] = []
move ((Bullet (V2 x y) (V2 vx vy) e):space) = (Bullet (V2 (x+vx) (y+vy)) (V2 vx vy) e):(move space)
move ((Asteroid (V2 x y) (V2 vx vy) s e):space) = (Asteroid (V2 (x+vx) (y+vy)) (V2 vx vy) s e):(move space)

distance::Object -> Object -> Bool
distance (Bullet (V2 x y) _ _) (Asteroid (V2 ax ay) _ s _) | sqrt((x-ax)**2 + (y-ay)**2) < bulletR+asteroidR*s = True
                                                           | otherwise = False
distance (Asteroid (V2 ax ay) _ s _) (Bullet (V2 x y) _ _) | sqrt((x-ax)**2 + (y-ay)**2) < bulletR+asteroidR*s = True
                                                           | otherwise = False
distance (Player (V2 x y) v a t e _) (Asteroid (V2 ax ay) _ s _) | sqrt((x-ax)**2 + (y-ay)**2) < 10+asteroidR*s = True
                                                               | otherwise = False

bounds::Space->Space
bounds ((Bullet (V2 x y) (V2 vx vy) e):space) | (x > 250) || (x <(-250)) || (y>250) || (y<(-250)) = [Bullet (V2 x y) (V2 vx vy) False] ++ bounds space
                                      | otherwise = [Bullet (V2 x y) (V2 vx vy) True] ++ bounds space
bounds ((Asteroid (V2 x y) (V2 vx vy) s e):space) = [Asteroid (V2 x' y') (V2 vx vy) s e] ++ bounds space where 
      x'  | x > 250 = -249
          | x < -250 = 249
          | otherwise = x
      y'  | y > 250 = -249
          | y < -250 = 249	
          | otherwise = y
bounds [] = []
bounds (s:space) = s:(bounds space)

collision'::Object->Space->Object
collision' obj@(Bullet (V2 x y) (V2 vx vy) e)
           ((Asteroid (V2 ax ay) _ s _):space) | cond = Bullet (V2 x y) (V2 vx vy) False 
                                               | otherwise = collision' obj space where
                                                  cond | distance (Bullet (V2 x y) (V2 vx vy) e)
                                                                  (Asteroid (V2 ax ay) (V2 0 0) s True) = True
                                                       | otherwise = False
collision' obj@(Player (V2 x y) v a t e j) 
           ((Asteroid (V2 ax ay) (V2 vx vy) s ae):space) | cond = Player (V2 x y) v a t False j	 
                                                         | otherwise = collision' obj space where
                                                            cond | distance (Player (V2 x y) v a t e j)
                                                                            (Asteroid (V2 ax ay) (V2 0 0) s True) = True
                                                                 | otherwise = False
collision' obj@(Asteroid (V2 x y) (V2 vx vy) s e) 
           ((Bullet (V2 bx by) _ True):space) | cond = Asteroid (V2 x y) (V2 vx vy) s False 
                                              | otherwise = collision' obj space where
                                                 cond | distance (Asteroid (V2 x y) (V2 vx vy) s e)
                                                                 (Bullet (V2 bx by) (V2 0 0) True) = True
                                                      | otherwise = False  
collision' obj [] = obj
collision' obj (s:space) = collision' obj space

playerUpdate::Object->Float->Float->Space->Object
playerUpdate (Player (V2 x y) v a t e j) vx vy space = collision' (Player (V2 (x'+vx) (y'+vy))  (max v' 0) a t e j) space where --проверка на выход за пределы экрана
      x'  | x > 250 = -249
          | x < -250 = 249
          | otherwise = x
      y'  | y > 250 = -249
          | y < -250 = 249	
          | otherwise = y
      v'  | j = v + accel
          | otherwise = v - decel   

collisionHandler::Space->Space->Space
collisionHandler [] _ = []
collisionHandler ((Bullet (V2 x y) (V2 vx vy) e):space) _ = ((Bullet (V2 x y) (V2 vx vy) False):space)
collisionHandler (s:rest) space = [collision' s space] ++ (collisionHandler rest space) 

cleanUp::Space->Space
cleanUp [] = []
cleanUp ((Bullet (V2 x y) (V2 vx vy) False):space) = cleanUp space
cleanUp ((Asteroid (V2 x y) (V2 vx vy) s False):space) | s > 1 = [Asteroid (V2 x y) (V2 (1.5*vx) (1.5*vy)) (s-1) True,
                                                                  Asteroid (V2 x y) (V2 ((-1.5)*vx) (1.5*vy)) (s-1) True] ++ (cleanUp space)
                                                       |otherwise = cleanUp space
cleanUp ((Player (V2 x y) v a t False j):space) = cleanUp space
cleanUp (s:space) = s:(cleanUp space)

stepGame :: Float -> Space -> IO Space
stepGame _ space@((Player (V2 x y) v a t False j):rest) = return space
stepGame _ ((Player (V2 x y) v a t e j):space) = do
   print $ space
   let V2 vx vy = (rotationMatrix a) !* (V2 v 0) --поворот вектора скорости
   return $ (playerUpdate (Player (V2 x y) v a t e j) vx vy space):(move $ cleanUp $ bounds $ collisionHandler space space) 
stepGame _ space = return space
