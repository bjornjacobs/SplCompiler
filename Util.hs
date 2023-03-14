{-# LANGUAGE TupleSections #-}
module Util where
import Data.Functor
-- Contains simple helper functions

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right y) = Right y

swapEither :: Either a b -> Either b a
swapEither (Left x) = Right x
swapEither (Right x) = Left x

sndCons :: b -> ([a], [b]) -> ([a], [b])
sndCons y (xs, ys) = (xs, y:ys)

fstCons :: a -> ([a], [b]) -> ([a], [b])
fstCons x (xs, ys) = (x:xs, ys)

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

flip3 :: (b -> c -> a -> r) -> a -> b -> c -> r
flip3 f a b c = f b c a

flip4 :: (b -> c -> d -> a -> r) -> a -> b -> c -> d -> r
flip4 f a b c d = f b c d a

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f = foldr g ([], [])
    where g a b = h (f a) b
          h (Left a) (x, y) = (a:x, y)
          h (Right b) (x, y) = (x, b:y)

firstM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
firstM f (a, b) = f a <&> (,b)

secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM f (a, b) = f b <&> (a,)

infixl 9 !!?
(!!?) :: [a] -> Int -> Maybe a

[] !!? _ = Nothing
(x:_) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i - 1)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e Nothing = Left e
maybeToEither _ (Just x) = Right x