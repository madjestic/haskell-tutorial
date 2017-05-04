module Main where

import Data.Char (toUpper)
import Control.Monad

-- | main = putStrLn "Write your string: " >> fmap shout getLine >>= putStrLn

-- | putStrLn "Write your string: " >> fmap shout getLine >>= putStrLn
-- |   
-- | could be written as:
-- |    
-- | do putStrLn "Write your string: "
-- |    string <- getLine
-- |    putStrLn (shout string)

-- | shout = map toUpper
   
main = undefined   
