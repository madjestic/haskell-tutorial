module Main where

-- | takeInt returns the first n items in a list. So, takeInt 4
-- | [11,21,31,41,51,61] returns [11,21,31,41]
   
takeInt _ 0 = []
takeInt (x:xs) n = x : takeInt xs (n - 1)
                               
-- | dropInt drops the first n items in a list and returns the rest. So,
-- | dropInt 3 [11,21,31,41,51] returns [41,51]

dropInt xs 0 = xs
dropInt (x:xs) n = dropInt xs (n - 1)
                           
-- | sumInt returns the sum of the items in a list.
   
sumInt [] = 0
sumInt (x:xs) = x + sumInt xs
                           
-- | scanSum adds the items in a list and returns a list of the running
-- | totals. So, scanSum [2,3,4,5] returns [2,5,9,14]
                           
scanSum (x:[]) = x:[]
scanSum (x:y:ys) = x : scanSum ((x + y) : ys)
                               
-- | diffs returns a list of the differences between adjacent items. So,
-- | diffs [3,5,6,8] returns [2,1,2]. (Hints: one solution involves writing
-- | an auxiliary function which takes two lists and calculates the
-- | difference between corresponding elements. Alternatively, you might
-- | explore the fact that lists with at least two elements can be matched
-- | to a (x:y:ys) pattern.)
   
diffs []       = []
diffs (x:y:[]) = (y - x) : []
diffs (x:y:ys) = (y - x) : diffs (y:ys)
                                 
                                 
-- | Use map to build functions that, given a list xs of Ints, return:
-- | A list that is the element-wise negation of xs.
-- | A list of lists of Ints xss that, for each element of xs, contains the divisors of xs. You can use the following function to get the divisors:
-- | divisors p = [ f | f <- [1..p], p `mod` f == 0 ]
-- | The element-wise negation of xss.                                 

main = undefined
