module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West
  deriving (Eq, Show, Read, Ord)

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1, y)
-- complete the definition

moves :: [Direction] -> Pos -> Pos
moves [] (x,y) = (x,y)
moves (a:xs) (x,y) =  moves xs (move a (x,y))
-- replace with actual definition of moves, and likewise for the
-- other 'undefined' functions

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ x) y = Succ (add x y)

mult :: Nat -> Nat -> Nat
mult Zero x = Zero
mult (Succ x) y = add y (mult x y)

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = 1 + nat2int x

int2nat :: Int -> Nat
int2nat 0 = Zero
-- int2nat (x + 1) = Succ (int2nat x)
int2nat x =  Succ (int2nat (x-1)) 


data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert n Leaf = Node n Leaf Leaf
insert n (Node x left right)
  | n > x = Node x left (insert n right)
  | n < x = Node x (insert n left) right
  | otherwise = Node x left right

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Eq, Show, Read, Ord)

--pinsert :: FIXME  -- uncomment and replace with the proper type of pinsert
pinsert :: Ord a => a -> PTree a -> PTree a
pinsert n PLeaf = PNode n PLeaf PLeaf
pinsert n (PNode b left right)
  | n > b = PNode b left (pinsert n right)
  | n < b = PNode b (pinsert n left) right
  | otherwise = PNode b left right
