{-# LANGUAGE Arrows #-}
module Main where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (position)
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import FRP.Netwire
import Data.IORef
import Linear


{-|
Размеры фигуры
-}
s :: Float
s = 0.05

{-|
  Ускорение
-}
speed :: Float
speed = 1.0
rot :: Float
rot = pi

{-|
Минимальная скорость
-}
minSpeed :: Float
minSpeed = 0.01

{-|
Начальная позиция
-}
initPos :: V2 Float
initPos = pure 0

{-|
проверяет нажатие кнопки
-}
isKeyDown :: (Monoid e, Enum k) => k -> Wire s e IO a a
isKeyDown k =
  mkGen_ $ \a -> do
    state <- getKey k
    return $ case state of
      Release -> Left  mempty
      Press   -> Right a

{-|
применяет функцию на вход вайра
-}
withInput :: (a -> b) -> Wire s e m a b
withInput fn = mkPure_ $ \a -> Right $ fn a

{-|
  превращает ускорение в замедление
-}
decel :: Float -> Float
decel x
  | x <  (-minSpeed)                     = ( speed)
  | x >  ( minSpeed)                     = (-speed)
  | otherwise                            = x

{-|
  Ускорение/замедление
-}
dAcceleration :: (Enum k, Monoid e) => k -> k -> Wire s e IO Float Float
dAcceleration k1 k2  =  withInput decel . isKeyDown k1 . isKeyDown k2
                    <|> pure ( speed)   . isKeyDown k1
                    <|> pure (-speed)   . isKeyDown k2
                    <|> withInput decel

rotationSpeed :: (Enum k, Monoid e) => k -> k -> Wire s e IO Float Float
rotationSpeed k1 k2  =   pure ( pi)   . isKeyDown k1
                    <|> pure (-pi)   . isKeyDown k2
                    <|> pure (0)  


rotationMatrix ::  Float -> V2 (V2 Float)
rotationMatrix a =  V2 (V2 (cos a) (- sin a)) (V2 (sin a) (cos a))


curAngle :: (HasTime t s, Monoid e) => Wire s e IO Float Float
curAngle = integral 0
{-|
  Скорость. При выходе скорости за минимальное ограничение сбрасывается в ноль, дабы фигура не ерзала
-}
velocity :: (HasTime t s, Monoid e) => Wire s e IO (Float, Float) (V2 Float)
velocity =
  withInput stop . integral 0 . withInput (uncurry V2)
  where stop :: V2 Float -> V2 Float
        stop = fmap stopF
          where stopF :: Float -> Float
                stopF x =
                  if x > (-minSpeed) && x < minSpeed
                    then 0
                    else x

{-|
Позиция и проверка выхода за край + перегрузка integralWith
-}
integralWith' ::
    (Fractional a, HasTime t s)
    => (a -> a)  -- Function for potentially limiting the integration
                      -- and producing a secondary output.
    -> a              -- Integration constant (aka start value).
    -> Wire s e m a a
integralWith' correct = loop
  where
    loop x' =
        mkPure $ \ds dx ->
            let dt = realToFrac (dtime ds)
                x  = correct (x' + dt*dx)
            in x' `seq` (Right x', loop x)


collided :: V2 Float -> V2 Float
collided (V2 x y)
  | x < -a = V2 (a-0.1) y 
  | x > a = V2 (-a+0.1) y
  | y < -b = V2 x (b-0.1)
  | y > b = V2 x (-b+0.1)  
  | otherwise = V2 x y
  where a = 1 + s
        b = 1 + s

position :: (HasTime t s, Monoid e) => Wire s e IO (V2 Float) (V2 Float)
position = integralWith' (collided) initPos 	

{-|
Связываем ускорение со скоростью, а скорость с позицией и возвращаем позицию
-}
fPos :: HasTime t s => Wire s () IO a (V2 Float)
fPos = proc _ -> do
  rec x            <- dAcceleration (CharKey 'D') (CharKey 'A') -< vx
      --y            <- dAcceleration (CharKey 'W') (CharKey 'S') -< vy
      r            <- rotationSpeed (CharKey 'W') (CharKey 'S') -< an
      an           <- curAngle -< r
      v@(V2 vx vy) <- velocity                                  -< (x, 0)
      p            <- position                                  -<  (rotationMatrix an) !* v
  returnA -< p

{-|
  Запускаем процесс, выходим при получении ошибки, иначе отрисовывем прямоугольник
-}
runNetwork' :: IORef Bool -> Session IO s -> Wire s e IO a (V2 Float) -> IO ()
runNetwork' closedRef session wire = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (st , session') <- stepSession session
      (dw', wire'   ) <- stepWire wire st $ Right undefined
      case dw' of
        Left  _      -> return ()
        Right (V2 x y) -> do
          clear [ColorBuffer]
          renderPrimitive Quads $
            mapM_ (\(V2 rx ry) -> vertex $ Vertex2 (realToFrac rx :: GLfloat)
                                                   (realToFrac ry :: GLfloat))
                  [ V2 (x - s) (y - s)
                  , V2 (x + s) (y - s)
                  , V2 (x + s) (y + s)
                  , V2 (x - s) (y + s)
                  ]

          swapBuffers

          runNetwork' closedRef session' wire'

{-|
вход
-}
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef =
  runNetwork' closedRef clockSession_ fPos

{-|
вход
-}
main :: IO ()
main = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "asteroids"

  closedRef <- newIORef False
  windowCloseCallback $= do
    writeIORef closedRef True
    return True

  runNetwork closedRef

  closeWindow

