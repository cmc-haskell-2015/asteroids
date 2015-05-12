
import Graphics.Gloss.Interface.IO.Game
import Linear.V2
import Linear.Matrix
import Data.Monoid (mconcat)
import System.Random
<<<<<<< HEAD

class Physical a where
   pos :: a -> (V2 Float)
   vel :: a -> (V2 Float)  
   ang :: a -> Float
   size :: a -> Float   

data Aux = Aux
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
data Space = Space Player [Bullet] [Asteroid] [Bonus]

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

vectorLength :: (V2 Float) -> Float
vectorLength (V2 x y) = sqrt $ x**2 + y**2

distance :: (Physical a, Physical b) => a -> b -> Float
distance x y = vectorLength $ pos x - pos y

player :: Player
player = Player (V2 0 0) 0 0 (Aux 0 False 0) True False

as :: [Asteroid]
as = [Asteroid (V2 100 100) (V2 (-0.1) 0) 4 True,
      Asteroid (V2 (-100) 120) (V2 (-1) 0.5) 3 True,
      Asteroid (V2 50 (-100)) (V2 2 (-1)) 2 True,
      Asteroid (V2 (-200) (-200)) (V2 2 4) 1 True]

initialSpace :: Space -- изначальное состояние, в нем только игрок
initialSpace =  Space player [] as []
=======
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
>>>>>>> 6a4d4770b5ad61b8b03103cd34594d1a882d1618

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
<<<<<<< HEAD

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

drawScore :: Player -> Picture
drawScore (Player _ _ _ (Aux t b c) _ _) = mconcat $ [translate (-240) (-240) . scale 0.1 0.1 . color white . text $ concat ["Score: ",show (100*c)]
                                                     ,drawBomb b
                                                     ,translate (190) (-240) . scale 0.1 0.1 . color white . text $ concat ["Time: ",show(fromIntegral(5*c) + t)]]

drawSpace :: Space -> IO Picture --отрисовывает все объекты, используя их коордианты
drawSpace (Space (Player (V2 x y) v a _ False _) _ _ _) = return $ scale 0.3 0.3 . translate (-400) 0 . color red . text $ "Game Over!"
drawSpace (Space p b as bon) = return $ mconcat $ [drawPlayer p
                                              , mconcat (map drawBullet b)
                                              , mconcat (map drawAsteroid as)
                                              , mconcat (map drawBonus bon)
                                              , drawScore p] 

--------------------------------------------------------------------------------
handleInput :: Event -> Space -> IO Space -- w - придание ускорения кораблю, d - поворот по часовой, a - против
handleInput (EventKey (Char 'w') Down _ _) 
            (Space (Player (V2 x y) v a t e j) b as bon) = return $ Space (Player (V2 x y) v a t e True) b as bon
handleInput (EventKey (Char 'w') Up _ _) 
            (Space (Player (V2 x y) v a t e j) b as bon) = return $ Space (Player (V2 x y) v a t e False) b as bon
handleInput (EventKey (Char 'a') Down _ _) 
            (Space (Player (V2 x y) v a t e j) b as bon) = return $ Space (Player (V2 x y) v (a+rotSpeed) t e j) b as bon
handleInput (EventKey (Char 'd') Down _ _) 
            (Space (Player (V2 x y) v a t e j) b as bon) = return $ Space (Player (V2 x y) v (a-rotSpeed) t e j) b as bon
handleInput (EventKey (Char 'x') Down _ _) 
            (Space (Player (V2 x y) v a (Aux t True c) e j) b as bon) = return $ Space (Player (V2 x y) v a (Aux t False c) e j) b [] bon
handleInput (EventKey (SpecialKey KeySpace) Down _ _) 
            (Space p b as bon) = return $ Space p (b ++ [Bullet (pos p) (rotateSpeed (ang p) 5) True]) as bon
handleInput  _ space = return space

collision :: (Physical a, Physical b) => a -> b-> Bool
collision a b = (distance a b) > (size a + size b) 

playerUpdate::Player->Float->Float->Float->[Asteroid]->[Bonus]->Player
playerUpdate p@(Player (V2 x y) (V2 v w) a (Aux t b c) e j) vx vy time as bon = Player (V2 (x'+vx) (y'+vy)) 
                                                                                       (V2 (max v' 0) w) a 
                                                                                       (Aux t' b' c') e' j where --проверка на выход за пределы экрана
=======
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
>>>>>>> 6a4d4770b5ad61b8b03103cd34594d1a882d1618
      x'  | x > 250 = -249
          | x < -250 = 24
          | otherwise = x
      y'  | y > 250 = -249
          | y < -250 = 249	
          | otherwise = y
<<<<<<< HEAD
      v'  | j = v + accel
          | otherwise = v - decel
      e'  | length as == 0 = True
          | otherwise = and $ map (\a -> collision p a) as
      t'  | t > 5 = 0
          | otherwise = t + time
      c'  | t > 5 = c + 1
          | otherwise = c
      b'  | b = b
          | otherwise = not $ and $ map (\a -> collision p a) bon    

bulletUpdate :: Bullet -> [Asteroid] -> Bullet
bulletUpdate b [] = b
bulletUpdate b@(Bullet (V2 x y) (V2 vx vy) e) as| (x > 250) || (x <(-250)) || (y>250) || (y<(-250)) = Bullet (V2 x y) (V2 vx vy) False
                                                | otherwise = Bullet (V2 (x+vx) (y+vy)) (V2 vx vy) (and $ map (\a -> collision b a) as)

asteroidUpdate :: Asteroid -> [Bullet] -> Asteroid
asteroidUpdate a@(Asteroid (V2 x y) (V2 vx vy) s e) b = Asteroid (V2 (x'+vx) (y'+vy)) (V2 vx vy) s (and $ map (\bul -> collision bul a) b) where 
                                                     x'  | x > 270 = -269
                                                         | x < -270 = 269
                                                         | otherwise = x
                                                     y'  | y > 270 = -269
                                                         | y < -270 = 269
                                                         | otherwise = y
bonusUpdate :: Bonus -> Player -> Bonus
bonusUpdate b@(Bonus pos e) p = Bonus pos e' where
    e' = collision b p

bulletCleanUp :: [Bullet] -> [Bullet]
bulletCleanUp [] = []
bulletCleanUp ((Bullet (V2 x y) (V2 vx vy) False):bs) = bulletCleanUp bs
bulletCleanUp (b:bs) = b:(bulletCleanUp bs)

asteroidCleanUp :: [Asteroid] -> [Asteroid]
asteroidCleanUp [] = []
asteroidCleanUp ((Asteroid (V2 x y) (V2 vx vy) s False):as) | s > 1 = [Asteroid (V2 x y) (V2 (1.5*vx) (1.5*vy)) (s-1) True,
                                                                  Asteroid (V2 x y) (V2 ((-1.5)*vx) (1.5*vy)) (s-1) True] ++ (asteroidCleanUp as)
                                                            |otherwise = asteroidCleanUp as
asteroidCleanUp (a:as) = a:(asteroidCleanUp as)

bonusCleanUp :: [Bonus] -> [Bonus]
bonusCleanUp [] = []
bonusCleanUp ((Bonus pos False):bs) = bonusCleanUp bs
bonusCleanUp (b:bs) = b:(bonusCleanUp bs)

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
addBonus (Player _ _ _ (Aux t _ c) _ _) x y| (t > 5) && (c `mod` 2 == 1) = [Bonus (V2 x y) True]
                                           | otherwise = []  
 
stepGame :: Float -> Space -> IO Space
stepGame time space@(Space (Player (V2 x y) v a t False j) _ _ _) = return space
stepGame t (Space p b as bon) = do
   g <- newStdGen
   let randPos = take 4 (randomRs ((-200)::Float,200::Float) g)
   let randV = take 2 (randomRs ((-3)::Float,3::Float) g)
   --print rand
   let V2 vx vy = (rotationMatrix (ang p)) !* (vel p) --поворот вектора скорости
   return $ Space (playerUpdate p vx vy t as bon) 
                  (bulletCleanUp $ map (\x -> bulletUpdate x as) b) 
                  ((asteroidCleanUp $ map (\x -> asteroidUpdate x b) as) ++ (addAsteroid p (randPos!!1) (randPos!!2) (randV!!0) (randV!!1) )) 
                  ((bonusCleanUp $ map (\x -> bonusUpdate x p) bon) ++ (addBonus p (randPos!!0) (randPos!!1) )) 
=======
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
>>>>>>> 6a4d4770b5ad61b8b03103cd34594d1a882d1618
