module Main where

import Asteroids.Types
import Asteroids.Core
import Asteroids.Utils

import Graphics.Gloss.Interface.IO.Game
import Linear.V2
import Linear.Matrix
import Data.Monoid (mconcat)
import System.Random

player :: Player
player = Player (V2 0 0) 0 0 (Aux 0 False 0) True False

ufo :: [UFO]
ufo = [UFO (V2 300 0) (V2 (-1) 0) 0 True 10 0 [0,pi/4,pi/2,3*pi/4,pi,5*pi/4,3*pi/2,7*pi/4]]

as :: [Asteroid]
as = [Asteroid (V2 100 100) (V2 (-0.1) 0) 4 True,
      Asteroid (V2 (-100) 120) (V2 (-1) 0.5) 3 True,
      Asteroid (V2 50 (-100)) (V2 2 (-1)) 2 True,
      Asteroid (V2 (-200) (-200)) (V2 2 4) 1 True]

initialSpace :: Space -- изначальное состояние, в нем только игрок
initialSpace =  Space player [] as [] [] []

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

drawPlayer :: Player -> Picture
drawPlayer (Player (V2 x y) v a _ _ _) = Color white $ translate x y $ rotate ((-180)*a/pi) $ polygon [(0, 10), (0, -10), (20, 0)]

drawBullet :: Bullet -> Picture
drawBullet (Bullet (V2 x y) _ _) = Color white $ translate x y $ Circle 5

drawAsteroid :: Asteroid -> Picture
drawAsteroid (Asteroid (V2 x y) _ s _) = Color white $ translate x y $ ThickCircle (5*s) (10*s)

drawBonus :: Bonus -> Picture
drawBonus (Bonus (V2 x y) _) = Color green $ translate x y $ polygon [(5, 5), (5, -5), (-5, -5), (-5,5)]

drawBomb:: Bool -> Picture
drawBomb b | b = Color green $ translate (-150) (-235) $ ThickCircle 5 10
           | otherwise = Color black $ translate (-150) (-200) $ ThickCircle 5 10

drawUFO :: UFO -> Picture
drawUFO (UFO (V2 x y) _ a _ _ _ _) = mconcat $ [Color white $ translate x y $ ThickCircle 25 50,
                                            Color red $ translate x y $ ThickCircle 10 20,
                                            Color black $ translate x' y' $ ThickCircle 4 8] where
                                                                    x' = x + vX ( (rotationMatrix a) !* (V2 0 10) )
                                                                    y' = y +vY ( (rotationMatrix a) !* (V2 0 10) )  
drawEnemyBullet :: EnemyBullet -> Picture
drawEnemyBullet (EnemyBullet (V2 x y) _ _) = Color blue $ translate x y $ ThickCircle 5 10

drawScore :: Player -> Picture
drawScore (Player _ _ _ (Aux t b c) _ _) = mconcat $ [translate (-240) (-240) . scale 0.1 0.1 . color white . text $ concat ["Score: ",show (100*c)]
                                                     ,drawBomb b
                                                     ,translate (190) (-240) . scale 0.1 0.1 . color white . text $ concat ["Time: ",show(fromIntegral(5*c) + t)]]
drawGameOver :: Float->Int->Picture
drawGameOver t c = mconcat $ [ scale 0.3 0.3 . translate (-400) 0 . color red . text $ "Game Over!"
                             , scale 0.1 0.1 . translate (-1150) (-200) . color red . text $ concat ["Score: ",show (100*c), "     Time: ",show(fromIntegral(5*c) + t)]]

drawSpace :: Space -> IO Picture --отрисовывает все объекты, используя их коордианты
drawSpace (Space (Player (V2 x y) v a (Aux t _ c) False _) _ _ _ _ _) = return $ drawGameOver t c
drawSpace (Space p b as bon ufo eb) = return $ mconcat $ [drawPlayer p
                                              , mconcat (map drawBullet b)
                                              , mconcat (map drawAsteroid as)
                                              , mconcat (map drawBonus bon)
                                              , mconcat (map drawUFO ufo)
                                              , mconcat (map drawEnemyBullet eb)
                                              , drawScore p] 

--------------------------------------------------------------------------------
handleInput :: Event -> Space -> IO Space -- w - придание ускорения кораблю, d - поворот по часовой, a - против, х - бомба
handleInput (EventKey (Char 'w') Down _ _) 
            (Space (Player (V2 x y) v a t e j) b as bon ufo eb) = return $ Space (Player (V2 x y) v a t e True) b as bon ufo eb
handleInput (EventKey (Char 'w') Up _ _) 
            (Space (Player (V2 x y) v a t e j) b as bon ufo eb) = return $ Space (Player (V2 x y) v a t e False) b as bon ufo eb
handleInput (EventKey (Char 'a') Down _ _) 
            (Space (Player (V2 x y) v a t e j) b as bon ufo eb) = return $ Space (Player (V2 x y) v (a+rotSpeed) t e j) b as bon ufo eb
handleInput (EventKey (Char 'd') Down _ _) 
            (Space (Player (V2 x y) v a t e j) b as bon ufo eb) = return $ Space (Player (V2 x y) v (a-rotSpeed) t e j) b as bon ufo eb
handleInput (EventKey (Char 'x') Down _ _) 
            (Space (Player (V2 x y) v a (Aux t True c) e j) b as bon ufo eb) = return $ Space (Player (V2 x y) v a (Aux t False c) e j) b [] bon ufo eb
handleInput (EventKey (SpecialKey KeySpace) Down _ _) 
            (Space p b as bon ufo eb) = return $ Space p (b ++ [Bullet (pos p) (rotateSpeed (ang p) 5) True]) as bon ufo eb
handleInput  _ space = return space
 
stepGame :: Float -> Space -> IO Space --основное тело программы
stepGame time space@(Space (Player (V2 x y) v a t False j) _ _ _ _ _) = return space
stepGame t (Space p b as bon ufo eb) = do
   g <- newStdGen
   let randPos = take 4 (randomRs ((-200)::Float,200::Float) g)
   let randV = take 2 (randomRs ((-3)::Float,3::Float) g)
   let V2 vx vy = (rotationMatrix (ang p)) !* (vel p) 
   return $ Space (playerUpdate p vx vy t as bon eb) 
                  (bulletCleanUp $ map (\x -> bulletUpdate x as ufo) b) 
                  ((asteroidCleanUp $ map (\x -> asteroidUpdate x b) as) ++ (addAsteroid p (randPos!!1) (randPos!!2) (randV!!0) (randV!!1) )) 
                  ((bonusCleanUp $ map (\x -> bonusUpdate x p) bon) ++ (addBonus p (randPos!!0) (randPos!!1) ))
                  ((ufoCleanUp $ map (\x -> ufoUpdate x b t) ufo) ++ (addUFO p))
                  ((enemyBulletCleanUp $ map (\x -> enemyBulletUpdate x p) eb) ++ (addManyEnemyBullets ufo p)) 
