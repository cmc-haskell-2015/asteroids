module Main where

import Asteroids.Types
import Asteroids.Core
import Asteroids.Utils

import Graphics.Gloss.Interface.IO.Game
import Linear.V2
import Linear.Matrix
import Data.Monoid (mconcat)
import System.Random

-- |Начальное состояние игрока ноль - по всем координатам, скорости и прошедшему времени.
initPlayer :: Player 
initPlayer = Player (V2 0 0) 0 0 (Aux 0 False 0) True False 

-- |Начальные характеристики НЛО.
initUFO :: [UFO] 
initUFO = [UFO (V2 300 0) (V2 (-1) 0) 0 True 10 0 [0,pi/4,pi/2,3*pi/4,pi,5*pi/4,3*pi/2,7*pi/4]]  

-- | Начальный массив астероидов.
initAs :: [Asteroid] 
initAs = [Asteroid (V2 100 100) (V2 (-0.1) 0) 4 True,
      Asteroid (V2 (-100) 120) (V2 (-1) 0.5) 3 True,
      Asteroid (V2 50 (-100)) (V2 2 (-1)) 2 True,
      Asteroid (V2 (-200) (-200)) (V2 2 4) 1 True]

-- | Изначальное состояние игры, в нем только игрок и начальные астероиды.
initialSpace :: Space 
initialSpace =  Space initPlayer [] initAs [] [] []

-- | Ускорение, прибавляется к скорости при нажатии w (скорость - пиксели*масштаб в секунду).
accel::Float 
accel = 0.1

-- | Радиус пули.
bulletR::Float 
bulletR = 5

-- | Радиус астероида.
asteroidR::Float 
asteroidR = 5

-- | Замедление, естественная убыль скорости.
decel::Float 
decel = 0.05

-- | Скорость поворота, радианы за нажатие.
rotSpeed::Float 
rotSpeed = pi/8
--------------------------------------------------------------------------------
-- | Входная точка.
main :: IO () 
main = do
    playIO
        (InWindow "Asteroids" (500, 500) (500, 500)) --Имя, размеры, место окна.
        black --Цвет фона.
        60 --FPS.
        initialSpace --Начальный мир.
        drawSpace --Функция прорисовки мира.
        handleInput --Обработчик нажатий клавиш.
        stepGame --Обработчик каждого кадра игры.

--------------------------------------------------------------------------------
-- | Отрисовка игрока в виде белого треугольника.
drawPlayer :: Player -> Picture 
drawPlayer (Player (V2 x y) _ a _ _ _) = 
    Color   white $ 
            translate x y $ 
            rotate ((-180)*a/pi) $ 
            polygon [(0, 10), (0, -10), (20, 0)]

-- | Отрисовка пули в виде белой окружности радиуса 5.
drawBullet :: Bullet -> Picture 
drawBullet (Bullet (V2 x y) _ _) = 
    Color   white $ 
            translate x y $ 
            Circle bulletR

-- | Отрисовка астреоида в виде белого круга радиуса 5*размер астероида.
drawAsteroid :: Asteroid -> Picture 
drawAsteroid (Asteroid (V2 x y) _ s _) = 
    Color   white $ 
            translate x y $ 
            ThickCircle (asteroidR*s) (2*asteroidR*s)

-- | Отрисовка бонуса в виде зеленого квадрата 10х10.
drawBonus :: Bonus -> Picture 
drawBonus (Bonus (V2 x y) _) = 
    Color   green $ 
            translate x y $ 
            polygon [(5, 5), (5, -5), (-5, -5), (-5,5)]

-- | Отрисовка наличия бомбы у игрока в виде зеленого круга на интерфейсе.
drawBomb:: Bool -> Picture 
drawBomb b  
    | b = Color green $ 
                translate ((-edgeRight)+100) ((-edgeTop)+15) $ 
                ThickCircle 5 10
    | otherwise = Color black $ 
                        translate ((-edgeRight)+100) ((-edgeTop)+15) $
                        ThickCircle 5 10

-- | Отрисовка НЛО в виде белого круга радиуса 25, с внутренним красным кругом радиуса 10, с внутренним вращающимся черным кругом радиуса 4.
drawUFO :: UFO -> Picture 
drawUFO (UFO (V2 x y) _ a _ _ _ _) =  
    mconcat $ [Color white $ translate x y $ ThickCircle 25 50,
                Color red $ translate x y $ ThickCircle 10 20,
                Color black $ translate x' y' $ ThickCircle 4 8] where
                    x' = x + vX ( (rotationMatrix a) !* (V2 0 10) )
                    y' = y + vY ( (rotationMatrix a) !* (V2 0 10) )

-- | Отрисовка вражеской пули в виде синего круга радиуса 5.  
drawEnemyBullet :: EnemyBullet -> Picture 
drawEnemyBullet (EnemyBullet (V2 x y) _ _) =
    Color   blue $ 
            translate x y $ 
            ThickCircle 5 10

-- | Отрисовка очков и бонусов игрока в нижнем правом углу 
drawScore :: Player -> Picture 
drawScore (Player _ _ _ (Aux t b c) _ _) = 
    mconcat $   [translate ((-edgeRight)+10) ((-edgeTop)+10) . 
                    scale 0.1 0.1 . 
                    color white . 
                    text $ 
                    concat ["Score: ",show (100*c)]
                ,drawBomb b
                ,translate (edgeRight-50) ((-edgeRight)+10) . 
                    scale 0.1 0.1 . 
                    color white . 
                    text $ 
                    concat ["Time: ",show(fromIntegral(5*c) + t)]
                ]

-- | Отрисовка экрана конца игры.
drawGameOver :: Time -> Cycle -> Picture 
drawGameOver t c = 
    mconcat $ [ scale 0.3 0.3 . 
                translate (-400) 0 . 
                color red . 
                text $ "Game Over!"
                , scale 0.1 0.1 . 
                translate (-1150) (-200) . 
                color red . 
                text $ 
                concat ["Score: ",show (100*c)
                , "     Time: ",show(fromIntegral(5*c) + t)]
               ] --Очки = 100*число циклов.

-- | Отрисовка всех объектов в космосе.
drawSpace :: Space -> IO Picture 
drawSpace (Space (Player _ _ _ (Aux t _ c) False _) _ _ _ _ _) = 
    return $ drawGameOver t c
drawSpace (Space p b as bon ufo eb) = 
    return $ mconcat $  [drawPlayer p
                        , mconcat (map drawBullet b)
                        , mconcat (map drawAsteroid as)
                        , mconcat (map drawBonus bon)
                        , mconcat (map drawUFO ufo)
                        , mconcat (map drawEnemyBullet eb)
                        , drawScore p] 

--------------------------------------------------------------------------------
-- | Обработка нажатий клавиш. w - придание ускорения кораблю, d - поворот по часовой, a - против, х - бомба.
handleInput :: Event -> Space -> IO Space 
handleInput (EventKey (Char 'w') Down _ _) -- w - Придание ускорения кораблю.
            (Space (Player (V2 x y) v a t e _) b as bon ufo eb) = 
                return $ Space (Player (V2 x y) v a t e True) b as bon ufo eb
handleInput (EventKey (Char 'w') Up _ _) -- При отпускании w корабль замедляется.
            (Space (Player (V2 x y) v a t e _) b as bon ufo eb) = 
                return $ Space (Player (V2 x y) v a t e False) b as bon ufo eb
handleInput (EventKey (Char 'a') Down _ _) -- a - Поворот против часовой.
            (Space (Player (V2 x y) v a t e j) b as bon ufo eb) = 
                return $ Space (Player (V2 x y) v (a+rotSpeed) t e j) b as bon ufo eb
handleInput (EventKey (Char 'd') Down _ _) -- d - Поворот по часовой.
            (Space (Player (V2 x y) v a t e j) b as bon ufo eb) = 
                return $ Space (Player (V2 x y) v (a-rotSpeed) t e j) b as bon ufo eb
handleInput (EventKey (Char 'x') Down _ _) -- x - Взрыв бомбы.
            (Space (Player (V2 x y) v a (Aux t True c) e j) b _ bon ufo eb) = 
                return $ Space (Player (V2 x y) v a (Aux t False c) e j) b [] bon ufo eb
handleInput (EventKey (SpecialKey KeySpace) Down _ _) -- Пробел - выстрел.
            (Space p b as bon ufo eb) = 
                return $ Space p (b ++ [Bullet (pos p) (rotateSpeed (ang p) 5) True]) as bon ufo eb
handleInput  _ space = return space

-- | Основное тело программы. 
stepGame :: Float -> Space -> IO Space 
stepGame _ space@(Space (Player _ _ _ _ False _) _ _ _ _ _) = return space
stepGame t (Space p b as bon ufo eb) = do
    g <- newStdGen
    let randPos = take 4 (randomRs ((-200)::Float,200::Float) g) -- Случайная позиция для астероидов и бомб. 
    let randV = take 2 (randomRs ((-3)::Float,3::Float) g) -- Случайная скорость для астероидов.
    let V2 vx vy = (rotationMatrix (ang p)) !* (vel p) -- Скорость игрока после поворота.
    return $ Space  (playerUpdate p vx vy t as bon eb) -- Обновление и чистка всех объектов в космосе. 
                    (bulletCleanUp $ map (\x -> bulletUpdate x as ufo) b) 
                    ((asteroidCleanUp $ map (\x -> asteroidUpdate x b) as) ++ 
                        (addAsteroid p (randPos!!1) (randPos!!2) (randV!!0) (randV!!1) )) 
                    ((bonusCleanUp $ map (\x -> bonusUpdate x p) bon) ++ 
                        (addBonus p (randPos!!0) (randPos!!1) ))
                    ((ufoCleanUp $ map (\x -> ufoUpdate x b t) ufo) ++ (addUFO p))
                    ((enemyBulletCleanUp $ map (\x -> enemyBulletUpdate x p) eb) ++ 
                        (addManyEnemyBullets ufo p)) 
