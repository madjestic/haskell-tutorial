module Main where

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)
-- | can be roughly re-written as:
-- |          Leaf   :: a -> Tree a
-- |                   (a -> b)   
-- |          Branch :: Tree a -> Tree a -> Tree a   
-- |                   (a -> a -> a)   
   
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)   

tree1 :: Tree Integer
tree1 = 
    Branch
       (Branch 
           (Branch 
               (Leaf 1) 
               (Branch (Leaf 2) (Leaf 3))) 
           (Branch 
               (Leaf 4) 
               (Branch (Leaf 5) (Leaf 6)))) 
       (Branch
           (Branch (Leaf 7) (Leaf 8)) 
           (Leaf 9))
       
treeMap :: (a -> b) -> Tree a -> Tree b
-- treeMap f (Leaf x) = Leaf (f x)
-- treeMap f (Branch x y) = Branch (treeMap f x) (treeMap f y)
treeMap f = g where
        g (Leaf x) = Leaf (f x)
        g (Branch left right ) = Branch (g left) (g right)

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold fBranch fLeaf = g where
         g (Leaf x)   = fLeaf x
         g (Branch left right) = fBranch (g left) (g right)
                                         
doubleTree = treeMap (*2)  -- doubles each value in tree
sumTree = treeFold (+) id -- sum of the leaf values in tree
fringeTree = treeFold (++) (: [])  -- list of the leaves of tree
                                                  
                                            
main = undefined
