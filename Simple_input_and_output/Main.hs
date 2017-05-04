module Main where

-- | Write a program which asks the user for the base and height of a 
-- | right angled triangle, calculates its area, and prints it to 
-- | the screen. The interaction should look something like:
-- |
-- | The base?
-- | 3.3
-- | The height?
-- | 5.4
-- | The area of that triangle is 8.91  

main :: IO ()
main = do
     putStrLn "The base?"
     b <- getLine
     putStrLn "The height?"
     h <- getLine
     print ("The are of the triangle is " ++ show (0.5 * read b * read h))

