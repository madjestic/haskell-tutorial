{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Char

cons8 :: [Integer] -> [Integer]
cons8 xs = xs ++ [8]

pack :: a -> [a]
pack x = [x]       

-- | I wonder if it's possible to use some typeclass magic to achieve
-- | the following:

-- foo :: (Bool, Int) -> IO ()
-- foo (a, b) = print b

-- foo :: (Bool, Int, String) -> IO ()
-- foo (a, b, c) = print c

-- | Use a combination of fst and snd to extract the 4 from the tuple (("Hello", 4), True)   
-- | (snd . fst) (("Hello", 4), True)
   
-- | Write a function which returns the head and the tail of a list 
-- | as the first and second elements of a tuple.   

headTail2fstSnd :: [a] -> (a, [a])
headTail2fstSnd xs = (head xs, tail xs)


-- | Use head and tail to write a function which gives the fifth 
-- | element of a list. Then, make a critique of it, pointing out 
-- | any annoyances and pitfalls you notice.
-- | (head . tail . tail . tail . tail) [1,2,3,4,5,6,7]
-- | (head (tail (tail (tail (tail [1,2,3,4,5,6,7])))))  
   
h :: Int -> t -> t1 -> Char
h x y z = chr (x - 2)   
  
x = 2
y = x + 1.1 

main :: IO ()
main = undefined
