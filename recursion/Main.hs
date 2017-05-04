module Main where

factorial 0 = 1
factorial n = n * factorial (n - 1)  
          
-- | The double factorial of a number n is the product of every other 
-- | number from 1 (or 2) up to n. For example, the double factorial 
-- | of 8 is 8 × 6 × 4 × 2 = 384, and the double factorial of 7 is 
-- | 7 × 5 × 3 × 1 = 105. Define a doublefactorial function in Haskell.  
   

dufactorial 0 = 1
dufactorial 1 = 1
dufactorial n = n * dufactorial (n - 2)
                                
-- | Define a recursive function power such that power x y raises x 
-- | to the y power.
   
power _ 0 = 1   
power m n = m * power m (n - 1)
                      
-- | (Harder) Implement the function log2, which computes the integer log
-- | (base 2) of its argument. That is, log2 computes the exponent of the
-- | largest power of 2 which is less than or equal to its argument. For
-- | example, log2 16 = 4, log2 11 = 3, and log2 1 = 0. (Small hint: read
-- | the last phrase of the paragraph immediately preceding these
-- | exercises.)
   
-- | we need to find: log2 m = n   , where m <= n, 2^n <= m
   
log2 :: Integer -> Integer
log2 m = log2' m 0

log2' :: Integer -> Integer -> Integer   
log2' m n
      | m > n && 2^n < m = log2' m (n + 1)
      | otherwise = n
        
log2'' 1 = 0
log2'' n = 1 + log2''(n `div` 2)


-- | replicate :: Int -> a -> [a], which takes a count and an element and
-- | returns the list which is that element repeated that many
-- | times. E.g. replicate 3 'a' = "aaa". (Hint: think about what replicate
-- | of anything with a count of 0 should be; a count of 0 is your 'base
-- | case'.)

repl :: t -> Int -> [t]
repl _ 0 = []        
repl x n = repl' x n []

repl' :: a -> Int -> [a] -> [a]
repl' x n q
      | length q < n = x : repl' x n (x:q)
      | otherwise = []
        
repl'' _ 0 = []
repl'' x n = x : repl'' x (n - 1)
                        
-- in my solutions I've been 'accumulating up', while that works in 
-- principle, the opposite approach, 'decrementing down' from n to 0
-- and catching the [] seems more efficient   



-- | (!!!) :: [a] -> Int -> a, which returns the element at the given
-- | 'index'. The first element is at index 0, the second at index 1, and
-- | so on. Note that with this function, you're recursing both numerically
-- | and down a list
   
idx :: (Num a, Num a1, Ord a1) => [a] -> a1 -> a
idx xs i = idx' xs i 0
       where
         idx' [] _ _ = (-1) -- error, empty list
         idx' (x:xs) i q
             | i > q = idx' xs i (q + 1)
             | otherwise = x

-- [] foobar _  = error "index too large"
-- (x:_) foobar 0  = x
-- (x:xs) foobar n = xs foobar (n - 1)
(!!!) []  _ = error "Index too large" -- An empty list has no elements.
(!!!) (x:_) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)   

-- | (A bit harder.) zip :: [a] -> [b] -> [(a, b)], which takes two lists
-- | and 'zips' them together, so that the first pair in the resulting list
-- | is the first two elements of the two lists, and so on. E.g. zip
-- | [1,2,3] "abc" = [(1, 'a'), (2, 'b'), (3, 'c')]. If either of the lists
-- | is shorter than the other, you can stop once either list runs
-- | out. E.g. zip [1,2] "abc" = [(1, 'a'), (2, 'b')]
       
zi :: [a] -> [b] -> [(a, b)]
zi _ [] = []
zi [] _ = []
zi (x:xs) (y:ys) = (x, y) : zi xs ys


main = do
  putStrLn "Hi"
