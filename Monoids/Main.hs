module Main where

-- | It turns out there are a great many associative binary operations with
-- | an identity. All of them, by definition, give us examples of
-- | monoids. We say, for instance, that the integer numbers form a monoid
-- | under addition with 0 as identity element.
   
-- | Monoid under addition.
newtype Sum a = Sum { getSum :: a }   
        
-- | Monoid under multiplication.
newtype Product a = Product { getProduct :: a }   
        
instance Num a => Monoid (Sum a) where
         mempty = Sum 0
         Sum x `mappend` Sum y = Sum (x + y)
                                     
instance Num a => Monoid (Product a) where
         mempty = Product 1
         Product x `mappend` Product y = Product (x * y)

main = undefined
