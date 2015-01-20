module HW07 where

import System.Random
import Data.List

fib :: Integer -> Integer
fib n 
	| n <= 0    = 0
	| n == 1    = 1
	| otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [ fib n |  n <- iterate (+1) 1 ]

fibs2 :: [Integer]
fibs2 = [0, 1] ++ subfib 0 1
	where subfib f1 f2 = f1 + f2 : subfib f2 (f1 + f2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
	show s = "[" ++ showTake 20 s ++ "]"
		where showTake n (Cons x xs)
			| n == 0    = "..."
			| otherwise =  show x ++ ", " ++ showTake (n-1) xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: ( a -> a ) -> a -> Stream a
streamFromSeed f s = Cons s $ streamFromSeed f $ f s

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons x xs) ys = Cons x $ interleaveStream ys xs
{-- interleaveStream (Cons x xs) (Cons y ys) = ... is not lazy version --}

ruler :: Stream Integer
ruler = f $ streamRepeat 0
	where f x = interleaveStream x . f . streamMap (+1) $ x

randomList :: (Random a, RandomGen g) => g -> [a]
randomList gen = r : randomList newgen
	where (r, newgen) = random gen

randomInts :: Int -> [Int]
randomInts n = take n $ randomList $ mkStdGen 83812039

minMax :: [Int] -> Maybe (Int ,Int)
minMax [] = Nothing
minMax xs = Just (minimum xs, maximum xs)

minMax2 :: [Int] -> Maybe (Int, Int)
minMax2 [] = Nothing
minMax2 (x:xs) = Just $ strictMinMax (x, x) xs
	where strictMinMax a [] = a
	      strictMinMax (mn, mx) (x:xs) = 
	      	mn `seq` mx `seq` strictMinMax (min mn x, max mx x) xs

data Mat2x2 = Mat2x2 Integer Integer Integer Integer

instance Num Mat2x2 where
	(Mat2x2 a b c d) * (Mat2x2 e f g h) = Mat2x2 (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

fib4 :: Integer -> Integer
fib4 n = x
	where (Mat2x2 _ x _ _) = (Mat2x2 1 1 1 0) ^ n