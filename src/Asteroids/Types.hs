-- | ???
module Asteroids.Types where

import Linear.V2

-- | Класс для обработки столкновений.
class Physical a where
  -- | Позиция.
  pos :: a -> (V2 Float)
  -- | Скорость.
  vel :: a -> (V2 Float)
  -- | Угол поворота.
  ang :: a -> Float
  -- | Размер.
  size :: a -> Float

-- | Информация о игре.
data Aux = Aux
   { timeA  :: Float   -- ^ Время.
   , bombA  :: Bool    -- ^ Наличие бомбы.
   , cycleA :: Int     -- ^ Прошедшее число циклов (1 цикл = 5 сек).
   } deriving (Show)

-- | Игрок
data Player = Player
   { positionP  :: (V2 Float)    -- ^ Позиция игрока.
   , speedP     :: (V2 Float)    -- ^ Скорость. 
   , angleP      :: Float        -- ^ Угол.
   , auxP        :: Aux          -- ^ Дополнительные данные.
   , existenceP :: Bool          -- ^ Жив ли?
   , jetP        :: Bool         -- ^ Включены ли движки?
   } deriving (Show)

-- | Пуля
data Bullet = Bullet 
   { positionB  :: (V2 Float)   -- ^ Позиция пули
   , speedB     :: (V2 Float)   -- ^ Скорость
   , existenceB :: Bool         -- ^ Жива ли?
   }

-- | Астероид
data Asteroid = Asteroid 
   { positionA  :: (V2 Float)   -- ^ Позиция.
   , speedA     :: (V2 Float)   -- ^ Скорость.
   , sizeA      :: Float        -- ^ Размер.
   , existenceA :: Bool         -- ^ Жив ли?
   }

-- | Бонус.
data Bonus = Bonus
   { positionBon  :: (V2 Float) -- ^ Позиция.
   , existenceBon :: Bool       -- ^ Жив ли?
   }

-- | НЛО.
data UFO = UFO
   { positionU  :: (V2 Float)   -- ^ Позиция.
   , speedU     :: (V2 Float)   -- ^ Скорость.
   , angleU     :: Float        -- ^ Угол.
   , existenceU :: Bool         -- ^ Существование.
   , hpU         :: Int          -- ^ Здоровье.
   , timeU      :: Float        -- ^ Время.
   , angles     :: [Float]      -- ^ Расположение турелей (в виде углов относительно окружности НЛО).
   }

-- | Вражеская пуля.
data EnemyBullet = EnemyBullet
   { positionE  :: (V2 Float)   -- ^ Позиция.
   , speedE     :: (V2 Float)   -- ^ Скорость.
   , existenceE :: Bool         -- ^ Существование.
   } 

-- | Космос.
data Space = Space Player [Bullet] [Asteroid] [Bonus] [UFO] [EnemyBullet]

-- | Угол.
type Angle = Float

-- | Время.
type Time = Float

type Cycle = Int

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

