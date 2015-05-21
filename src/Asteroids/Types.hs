module Asteroids.Types where

import Linear.V2

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

