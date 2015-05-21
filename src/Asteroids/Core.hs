module Asteroids.Core where

import Asteroids.Types
import Asteroids.Utils

import Linear.V2
import Linear.Matrix

accel::Float --ускорение, прибавляется к скорости при нажатии d (скорость - пиксели*масштаб в секунду)
accel = 0.1

decel::Float --замедление, естественная убыль скорости
decel = 0.05

collision :: (Physical a, Physical b) => a -> b-> Bool
collision a b = (distance a b) > (size a + size b) 

playerUpdate::Player->Float->Float->Float->[Asteroid]->[Bonus]->[EnemyBullet]->Player --движения + обработка столкновения игрока с пулями и астеродиами
playerUpdate p@(Player (V2 x y) (V2 v w) a (Aux t b c) e j) vx vy time as bon eb = Player (V2 (x'+vx) (y'+vy)) 
                                                                                       (V2 (max v' 0) w) a 
                                                                                       (Aux t' b' c') e' j where --проверка на выход за пределы экрана
      x'  | x > 250 = -249
          | x < -250 = 24
          | otherwise = x
      y'  | y > 250 = -249
          | y < -250 = 249	
          | otherwise = y
      v'  | j = v + accel
          | otherwise = v - decel
      e'  | (length as == 0) && (length eb == 0)  = True
          | otherwise = and [and $ map (\a -> collision p a) as
                            ,and $ map (\a -> collision p a) eb]
      t'  | t > 5 = 0
          | otherwise = t + time
      c'  | t > 5 = c + 1
          | otherwise = c
      b'  | b = b
          | otherwise = not $ and $ map (\a -> collision p a) bon    

bulletUpdate :: Bullet -> [Asteroid] -> [UFO] -> Bullet -- движение + проверка выхода за экран + столкновение с астероидами
bulletUpdate (Bullet (V2 x y) (V2 vx vy) e) [] []  = Bullet (V2 (x+vx) (y+vy)) (V2 vx vy) True
bulletUpdate b@(Bullet (V2 x y) (V2 vx vy) e) as ufo| (x > 250) || (x <(-250)) || (y>250) || (y<(-250)) = Bullet (V2 x y) (V2 vx vy) False
                                                    | otherwise = Bullet (V2 (x+vx) (y+vy)) (V2 vx vy) (and [and $ map (\a -> collision b a) as
                                                                                                           ,and $ map (\a -> collision b a) ufo])

asteroidUpdate :: Asteroid -> [Bullet] -> Asteroid --движение + проверка выхода за экран + столкновение с пулями
asteroidUpdate a@(Asteroid (V2 x y) (V2 vx vy) s e) b = Asteroid (V2 (x'+vx) (y'+vy)) (V2 vx vy) s (and $ map (\bul -> collision bul a) b) where 
                                                     x'  | x > 270 = -269
                                                         | x < -270 = 269
                                                         | otherwise = x
                                                     y'  | y > 270 = -269
                                                         | y < -270 = 269
                                                         | otherwise = y
bonusUpdate :: Bonus -> Player -> Bonus --проверка столкновений с игроком
bonusUpdate b@(Bonus pos e) p = Bonus pos e' where
    e' = collision b p

ufoUpdate :: UFO -> [Bullet] -> Float -> UFO --движения, столкновения и логика летающей тарелки
ufoUpdate ufo@(UFO (V2 x y) (V2 vx vy) a e hp t tur)  b time = UFO (V2 x' y') (V2 vx vy) a' e' hp' t' tur' where
                                                     x' | x <= 25 = x
                                                        | otherwise = x+vx
                                                     y' | y <= 0 = y
                                                        | otherwise = y+vy
                                                     hp'| (length b > 0) && (not $ and $ map (\a -> collision ufo a) b) = hp - 1
                                                        | otherwise = hp
                                                     e' | hp > 0 = True
                                                        | otherwise = False
                                                     tur' = map (+a) tur
                                                     a' | a > 2*pi = a - 2*pi
                                                        | otherwise = a+0.05
                                                     t' | t>1 = 0
                                                        | otherwise = t+time

enemyBulletUpdate :: EnemyBullet -> Player -> EnemyBullet --движение и столкновение пуль тарелки
enemyBulletUpdate eb@(EnemyBullet (V2 x y) (V2 vx vy) e) p | (x > 250) || (x <(-250)) || (y>250) || (y<(-250)) = EnemyBullet (V2 x y) (V2 vx vy) False
                                                           | otherwise = EnemyBullet (V2 (x+vx) (y+vy)) (V2 vx vy) (collision eb p)      
bulletCleanUp :: [Bullet] -> [Bullet] --очистка экрана от "умерших" пуль
bulletCleanUp [] = []
bulletCleanUp ((Bullet (V2 x y) (V2 vx vy) False):bs) = bulletCleanUp bs
bulletCleanUp (b:bs) = b:(bulletCleanUp bs)

enemyBulletCleanUp :: [EnemyBullet] -> [EnemyBullet] --очистка экрана от "умерших" вражеских пуль
enemyBulletCleanUp [] = []
enemyBulletCleanUp ((EnemyBullet (V2 x y) (V2 vx vy) False):bs) = enemyBulletCleanUp bs
enemyBulletCleanUp (b:bs) = b:(enemyBulletCleanUp bs)

asteroidCleanUp :: [Asteroid] -> [Asteroid] --очистка экрана от "умерших" астероидов + порождение мелких из трупов больших
asteroidCleanUp [] = []
asteroidCleanUp ((Asteroid (V2 x y) (V2 vx vy) s False):as) | s > 1 = [Asteroid (V2 x y) (V2 (1.5*vx) (1.5*vy)) (s-1) True,
                                                                  Asteroid (V2 x y) (V2 ((-1.5)*vx) (1.5*vy)) (s-1) True] ++ (asteroidCleanUp as)
                                                            |otherwise = asteroidCleanUp as
asteroidCleanUp (a:as) = a:(asteroidCleanUp as)

bonusCleanUp :: [Bonus] -> [Bonus] --очистка экрана от подобранных бонусов
bonusCleanUp [] = []
bonusCleanUp ((Bonus pos False):bs) = bonusCleanUp bs
bonusCleanUp (b:bs) = b:(bonusCleanUp bs)

ufoCleanUp :: [UFO] -> [UFO] --очистка экрана от "умерших" НЛО
ufoCleanUp [] = []
ufoCleanUp ((UFO _ _ _ False _ _ _):us) = ufoCleanUp us
ufoCleanUp (u:us) = u:(ufoCleanUp us) 

addAsteroid :: Player -> Float -> Float -> Float -> Float -> [Asteroid] 
addAsteroid (Player (V2 px py) _ _ (Aux t _ c) _ _) x y vx vy| (t>5) = [Asteroid (V2 x' y') (V2 vx' vy') s' True]
                                                             | otherwise = [] where
                                                               x'|x - px < 100 = x+100
                                                                 |px - x < 100 = x-100
                                                               y'|y - py < 100 = y+100
                                                                 |py - y < 100 = y-100
                                                               vx'|vx > 0 = vx + 0.1*(fromIntegral c)
                                                                  |vx < 0 = vx - 0.1*(fromIntegral c)
                                                               vy'|vy > 0 = vy + 0.1*(fromIntegral c)
                                                                  |vy < 0 = vy - 0.1*(fromIntegral c)
                                                               s' = fromIntegral (2 + (c `mod` 4))

addBonus:: Player->Float->Float->[Bonus]
addBonus (Player _ _ _ (Aux t _ c) _ _) x y| (t > 5) && (c `mod` 4 == 3) = [Bonus (V2 x y) True]
                                           | otherwise = []  

addHomingBullet :: (V2 Float) -> Player -> EnemyBullet
addHomingBullet (V2 x y) (Player (V2 px py) _ _ (Aux _ _ c) _ _) = EnemyBullet (V2 x y) (V2 vx vy) True where
                                                         vx = (2 + fromIntegral(c `mod` 6))*(px - x)/(vectorLength v)
                                                         vy = (2 + fromIntegral(c `mod` 6))*(py - y)/(vectorLength v)  
                                                         v = V2 (px - x) (py -y)

addEnemyBullet :: Float -> (V2 Float) -> Int -> EnemyBullet
addEnemyBullet turn p@(V2 x y) c = EnemyBullet p' v True where
                              p' = p + ((rotationMatrix (turn)) !* (V2 0 40))
                              v =  mulVector ((rotationMatrix (turn)) !* (V2 0 3)) (0.5*fromIntegral (c `mod` 6))

addEnemyBullets :: UFO -> Player -> [EnemyBullet]
addEnemyBullets (UFO pos@(V2 x y) _ a _ _ t tur) 
                p@(Player _ _ _ (Aux _ _ c) _ _)| (t > 0.5 - 0.1*fromIntegral(c `mod` 6)) && (x > 25) = [addHomingBullet (V2 x y) p]
                                                | (t > 0.95) && (x <= 25) = map (\x -> addEnemyBullet x pos c) tur
                                                |otherwise = []

addManyEnemyBullets :: [UFO] -> Player -> [EnemyBullet]
addManyEnemyBullets [] _ = []
addManyEnemyBullets (u:us) p = (addEnemyBullets u p) ++ (addManyEnemyBullets us p) 

addUFO :: Player -> [UFO]
addUFO (Player _ _ _ (Aux t _ c) _ _) | (t > 5) && (c `mod` 6 == 5) = [UFO (V2 300 0) (V2 (-1) 0) 0 True 10 0 [0,pi/4,pi/2,3*pi/4,pi,5*pi/4,3*pi/2,7*pi/4]]
                                      | otherwise = [] 

