module Main where

-- | So, >>= helps us pass non-monadic values to functions without leaving
-- | a monad. In the case of the Maybe monad, the monadic aspect is the
-- | qualifier that we don't know with certainty whether the value will be
-- | found.

-- | let x = foo in x + 3          corresponds to      (\x -> x + 3) foo
-- | x <- foo; return (x + 3)      to     foo >>= (\x -> return (x + 3))

displayResult :: Maybe Int -> String
displayResult mx = maybe "There was no result" (("The result was " ++) . show) mx   
              
-- | fromMaybe 0 (Just 1) - interesting example of Maybe and extracting
-- | values from Just   
   

main = undefined
