
import Graphics.Gloss.Interface.IO.Game
import Linear.V2
import Linear.Matrix
import Data.Monoid (mconcat)
import System.Random

class Physical a where --класс для обработки столкновений
   pos :: a -> (V2 Float)
   vel :: a -> (V2 Float)  
   ang :: a -> Float
   size :: a -> Float   

data Aux = Aux --информация о игре
   { time :: Float
   , bomb :: Bool
   , cycle :: Int} deriving (Show)  

data Player = Player 
   { positionP  :: (V2 Float)
   , speedP   :: (V2 Float)
   , angle :: Float 
   , aux :: Aux
   , existenceP :: Bool
   , jet :: Bool} deriving (Show)
data Bullet = Bullet 
   { positionB :: (V2 Float) 
   , speedB :: (V2 Float) 
   , existenceB :: Bool}
data Asteroid = Asteroid 
   { positionA :: (V2 Float) 
   , speedA :: (V2 Float) 
   , sizeA :: Float 
   , existenceA :: Bool} 
data Bonus = Bonus
   { positionBon :: (V2 Float)
   , existenceBon :: Bool
   }
data UFO = UFO
   { positionU :: (V2 Float)
   , speedU :: (V2 Float)
   , angleU :: Float
   , existenceU :: Bool
   , hp :: Int
   , timeU :: Float
   , angles :: [Float]
   }
data EnemyBullet = EnemyBullet
   { positionE :: (V2 Float)
   , speedE :: (V2 Float)
   , existenceE :: Bool} 
data Space = Space Player [Bullet] [Asteroid] [Bonus] [UFO] [EnemyBullet]

instance Physical Player where
   pos (Player p _ _ _ _ _) = p
   vel (Player _ v _ _ _ _) = v
   ang (Player _ _ a _ _ _) = a
   size _ = 10

instance Physical Bullet where
   pos (Bullet p _ _) = p
   vel (Bullet _ v _) = v
   size _ = 10
   ang _ = 0

instance Physical Asteroid where
   pos (Asteroid p _ _ _) = p
   vel (Asteroid _ v _ _) = v
   size (Asteroid _ _ s _) = 10*s
   ang _ = 0

instance Physical Bonus where
   pos (Bonus p _) = p
   vel _ = V2 0 0
   size _ = 10
   ang _ = 0

instance Physical EnemyBullet where
   pos (EnemyBullet p _ _) = p
   vel (EnemyBullet _ v _) = v
   size _ = 10
   ang _ = 0

instance Physical UFO where
   pos (UFO p _ _ _ _ _ _) = p
   vel (UFO _ v _ _ _ _ _) = v
   ang (UFO _ _ a _ _ _ _) = a
   size _ = 50

vectorLength :: (V2 Float) -> Float
vectorLength (V2 x y) = sqrt $ x**2 + y**2

mulVector :: (V2 Float) -> Float -> (V2 Float)
mulVector (V2 x y) k = V2 (k*x) (k*y) 

distance :: (Physical a, Physical b) => a -> b -> Float
distance x y = vectorLength $ pos x - pos y

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

vX :: (V2 a) -> a
vX (V2 x y) = x

vY :: (V2 a) -> a
vY (V2 x y) = y
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
