module Main where

data Tree a = Node a [Tree a]

instance Functor Tree where
--       fmap f (Node x ts) = Node (f x) (fmap (fmap f) ts)
         fmap f (Node x ts) = Node (f x) (fmap f <$> ts)
                                   

main = undefined
