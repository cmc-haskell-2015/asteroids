-- | ???
module Asteroids.Core where

import Asteroids.Types
import Asteroids.Utils

import Linear.V2
import Linear.Matrix

-- | Ускорение, прибавляется к скорости при нажатии @d@ (скорость - пиксели*масштаб в секунду).
accel :: Float
accel = 0.1

-- | Замедление, естественная убыль скорости.
decel :: Float
decel = 0.05

-- | Множитель скорости астероидов, появившихся из более крупного.
asteroidAccel :: Float 
asteroidAccel = 1.5

edgeTop::Float -- | Верхний край экрана.
edgeTop = 250

edgeRight::Float -- | Правый край экрана.
edgeRight = 250

-- | Небходимое расстояние между игроком и новоявленным астероидом.
playerOffset :: Float 
playerOffset = 100

-- | Увеличение скорости враждебных объектов со временем
speedRate :: Float 
speedRate = 0.1

-- | количество циклов, нужных для появления бонуса
bonusTime :: Int 
bonusTime = 4

-- | количество циклов, нужных для появления НЛО
ufoTime :: Int
ufoTime = 6

-- | время на один цикл 
cycleTime :: Float
cycleTime = 5

-- | Не столкнулись ли два объекта?
collision :: (Physical a, Physical b) => a -> b -> Bool
collision a b = (distance a b) > (size a + size b) 

-- | Движения + обработка столкновения игрока с пулями и астеродиами. Аргументы - Игрок, скорость по обоим направлениям, угол поворота, астероиды, бонусы, пули.
playerUpdate::Player -> Float -> Float -> Float -> [Asteroid] -> [Bonus] -> [EnemyBullet] -> Player
playerUpdate p@(Player (V2 x y) (V2 v w) a (Aux t b c) _ j) vx vy time as bon eb =
    Player (V2 (x'+vx) (y'+vy)) (V2 (max v' 0) w) a (Aux t' b' c') e' j where -- Проверка на выход за пределы экрана, столкновение с пулями и астероидами.
      x'  | x > edgeRight = (-edgeRight) + 1
          | x < (-edgeRight) = edgeRight - 1
          | otherwise = x
      y'  | y > edgeTop = (-edgeTop) + 1
          | y < (-edgeTop) = edgeTop - 1	
          | otherwise = y
      v'  | j = v + accel
          | otherwise = v - decel
      e'  | (length as == 0) && (length eb == 0)  = True
          | otherwise = and [and $ map (\a' -> collision p a') as
                            ,and $ map (\a' -> collision p a') eb]
      t'  | t > cycleTime = 0
          | otherwise = t + time
      c'  | t > cycleTime = c + 1
          | otherwise = c
      b'  | b = b
          | otherwise = not $ and $ map (\a' -> collision p a') bon    

-- | Движение + проверка выхода за экран + столкновение с астероидами.
bulletUpdate :: Bullet -> [Asteroid] -> [UFO] -> Bullet
bulletUpdate (Bullet (V2 x y) (V2 vx vy) _) [] []  = 
    Bullet (V2 (x+vx) (y+vy)) (V2 vx vy) True
bulletUpdate b@(Bullet (V2 x y) (V2 vx vy) _) as ufo
    | outside = Bullet (V2 x y) (V2 vx vy) False
    | otherwise = Bullet (V2 (x+vx) (y+vy)) (V2 vx vy) e' where
        outside = (x > edgeRight) || (x <(-edgeRight)) || (y>edgeTop) || (y<(-edgeTop))
        e' =  (and [and $ map (\a -> collision b a) as
                ,and $ map (\a -> collision b a) ufo])

-- | Движение + проверка выхода за экран + столкновение с пулями.
asteroidUpdate :: Asteroid -> [Bullet] -> Asteroid
asteroidUpdate a@(Asteroid (V2 x y) (V2 vx vy) s _) b = 
    Asteroid (V2 (x'+vx) (y'+vy)) (V2 vx vy) s (and $ map (\bul -> collision bul a) b) where 
        x'  | x > edgeRight + 20 = (-edgeRight) - 15
            | x < (-edgeRight) - 20 = edgeRight + 15
            | otherwise = x
        y'  | y > edgeTop + 20 = (-edgeTop) - 15
            | y < (-edgeTop) - 20 = edgeTop + 15
            | otherwise = y

-- | Проверка столкновений с игроком.
bonusUpdate :: Bonus -> Player -> Bonus
bonusUpdate b@(Bonus pos' _) p = Bonus pos' e' where
    e' = collision b p

-- | Движение, столкновения и логика летающей тарелки.
ufoUpdate :: UFO -> [Bullet] -> Float -> UFO
ufoUpdate ufo@(UFO (V2 x y) (V2 vx vy) a _ hp t tur) b time = 
    UFO (V2 x' y') (V2 vx vy) a' e' hp' t' tur' where 
        x'  | x <= 25 = x -- двигается до центра, после чего останавливается
            | otherwise = x+vx
        y'  | y <= 0 = y
            | otherwise = y+vy
        hp' | (length b > 0) && (not $ and $ map (\an -> collision ufo an) b) = hp - 1
            | otherwise = hp
        e'  | hp > 0 = True
            | otherwise = False
        tur' = map (+a) tur
        a'  | a > 2*pi = a - 2*pi
            | otherwise = a+0.05
        t'  | t>1 = 0
            | otherwise = t+time

-- | Движение и столкновение пуль тарелки.
enemyBulletUpdate :: EnemyBullet -> Player -> EnemyBullet
enemyBulletUpdate eb@(EnemyBullet (V2 x y) (V2 vx vy) _) p 
    |outside = EnemyBullet (V2 x y) (V2 vx vy) False
    |otherwise = EnemyBullet (V2 (x+vx) (y+vy)) (V2 vx vy) (collision eb p) where
        outside = (x > edgeRight) || (x <(-edgeRight)) || (y>edgeTop) || (y<(-edgeTop))

-- | Очистка экрана от "умерших" пуль.
bulletCleanUp :: [Bullet] -> [Bullet]
bulletCleanUp [] = []
bulletCleanUp ((Bullet _ _ False):bs) = bulletCleanUp bs
bulletCleanUp (b:bs) = b:(bulletCleanUp bs)

-- | Очистка экрана от "умерших" вражеских пуль.
enemyBulletCleanUp :: [EnemyBullet] -> [EnemyBullet]
enemyBulletCleanUp [] = []
enemyBulletCleanUp ((EnemyBullet _ _ False):bs) = enemyBulletCleanUp bs
enemyBulletCleanUp (b:bs) = b:(enemyBulletCleanUp bs)

-- | Очистка экрана от "умерших" астероидов + порождение мелких из трупов больших.
asteroidCleanUp :: [Asteroid] -> [Asteroid]
asteroidCleanUp [] = []
asteroidCleanUp ((Asteroid (V2 x y) (V2 vx vy) s False):as) 
    | s > 1 = [Asteroid (V2 x y) (V2 (asteroidAccel*vx) (asteroidAccel*vy)) (s-1) True -- Если размер астероида больше 1, то он раздваивается на более мелкие.
              ,Asteroid (V2 x y) (V2 ((-asteroidAccel)*vx) (asteroidAccel*vy)) (s-1) True
              ] ++ (asteroidCleanUp as)
    |otherwise = asteroidCleanUp as
asteroidCleanUp (a:as) = a:(asteroidCleanUp as)

-- | Очистка экрана от подобранных бонусов.
bonusCleanUp :: [Bonus] -> [Bonus]
bonusCleanUp [] = []
bonusCleanUp ((Bonus _ False):bs) = bonusCleanUp bs
bonusCleanUp (b:bs) = b:(bonusCleanUp bs)

-- | Очистка экрана от "умерших" НЛО.
ufoCleanUp :: [UFO] -> [UFO]
ufoCleanUp [] = []
ufoCleanUp ((UFO _ _ _ False _ _ _):us) = ufoCleanUp us
ufoCleanUp (u:us) = u:(ufoCleanUp us) 

-- | Добавление астероидов на экран (раз в 5 сек, 1 цикл).
addAsteroid :: Player -> Float -> Float -> Float -> Float -> [Asteroid] -- Аргументы: игрок, позиция астероида, скорость. 
addAsteroid (Player (V2 px py) _ _ (Aux t _ c) _ _) x y vx vy
    | (t>5) = [Asteroid (V2 x' y') (V2 vx' vy') s' True]
    | otherwise = [] where --проверка на то, чтобы астероиды не появились в игроке и увеличение средней скорости со временем
        x'  |x - px < playerOffset = x + playerOffset
            |px - x < playerOffset = x - playerOffset
            |otherwise = x
        y'  |y - py < playerOffset = y + playerOffset
            |py - y <= playerOffset = y - playerOffset
            |otherwise = y
        vx' |vx > 0 = vx + speedRate*(fromIntegral c)
            |otherwise = vx - speedRate*(fromIntegral c)
        vy' |vy > 0 = vy + speedRate*(fromIntegral c)
            |otherwise = vy - speedRate*(fromIntegral c)
        s' = fromIntegral (1 + (c `mod` bonusTime))

-- | Добавление бонусов (раз в 20 сек, 4 циклов).
addBonus:: Player -> Float -> Float -> [Bonus]
addBonus (Player _ _ _ (Aux t _ c) _ _) x y
    | (t > 5) && (c `mod` bonusTime == bonusTime - 1) = [Bonus (V2 x y) True]
    | otherwise = []  

-- | Выстрел самонаводящимся лучом по позиции игрока. 
addHomingBullet :: (V2 Float) -> Player -> EnemyBullet
addHomingBullet (V2 x y) (Player (V2 px py) _ _ (Aux _ _ c) _ _) = 
    EnemyBullet (V2 x y) (V2 vx vy) True where -- Ускоряются с течением времени.
        vx = (2 + fromIntegral(c `mod` ufoTime))*(px - x)/(vectorLength v)
        vy = (2 + fromIntegral(c `mod` ufoTime))*(py - y)/(vectorLength v)  
        v = V2 (px - x) (py -y)

-- | Выстрел вражеской пулей (стреляют по окружности, ускоряются со временем).
addEnemyBullet :: Angle -> (V2 Float) -> Cycle -> EnemyBullet
addEnemyBullet turn p c = 
    EnemyBullet p' v True where
        p' = p + ((rotationMatrix (turn)) !* (V2 0 40)) -- Турели лежат на внешней окружности.
        v =  mulVector  ((rotationMatrix (turn)) !* (V2 0 3)) 
                        (5*speedRate*fromIntegral (c `mod` ufoTime))

-- | Обработка всех выстрелов НЛО (скорострельность повышается со временем).
addEnemyBullets :: UFO -> Player -> [EnemyBullet]
addEnemyBullets (UFO pos'@(V2 x y) _ _ _ _ t tur) 
    p@(Player _ _ _ (Aux _ _ c) _ _)
        | (t > 0.5 - speedRate*fromIntegral(c `mod` ufoTime)) && (x > 25) = 
            [addHomingBullet (V2 x y) p] -- Половина секунды уходит на прицельный обстрел.
        | (t > 0.95) && (x <= 25) = map (\x' -> addEnemyBullet x' pos' c) tur --0.05 секунды уходит на тотальный обстрел.
        |otherwise = []

-- | Обработка всех выстрелов всех НЛО.
addManyEnemyBullets :: [UFO] -> Player -> [EnemyBullet]
addManyEnemyBullets [] _ = []
addManyEnemyBullets (u:us) p = (addEnemyBullets u p) ++ (addManyEnemyBullets us p) 

-- | Добавление НЛО (раз в 30 сек, 6 циклов).
addUFO :: Player -> [UFO]
addUFO (Player _ _ _ (Aux t _ c) _ _) 
    | (t > cycleTime) && (c `mod` ufoTime == ufoTime - 1) = 
        [UFO (V2 300 0) (V2 (-1) 0) 0 True 10 0 
            [0,pi/4,pi/2,3*pi/4,pi,5*pi/4,3*pi/2,7*pi/4]
        ]
    | otherwise = [] 

