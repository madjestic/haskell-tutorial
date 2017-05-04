module Main where
-- | Could we use what we've seen in this chapter so far to reduce this clumsiness?
-- | Declare a Date type which is composed of three Int, corresponding to
-- | year, month and day. Then, rewrite showDate so that it uses the new
-- | Date data type. What changes will then be needed in showAnniversary
-- | and the Anniversary for them to make use of Date?.   

data Anniversary = Birthday String Date       -- name, date (year, month, day)
                 | Wedding String String Date -- spouse name 1, spouse name 2, date (year, month, day)

data Date  = Date Year Month Day
type Year  = Int
type Month = Int
type Day   = Int             

showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name date) =
   name ++ " born " ++ showDate date

showAnniversary (Wedding name1 name2 date) =
   name1 ++ " married " ++ name2 ++ " on " ++ showDate date   
   


main = undefined   
