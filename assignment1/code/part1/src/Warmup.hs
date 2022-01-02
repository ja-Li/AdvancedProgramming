module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
-- complete the definition
move South (x,y) = (x, y-1)
move East  (x,y) = (x+1, y)

-- d is Direction, (xNew,yNew) is the position after the move.
moves :: [Direction] -> Pos -> Pos
moves [] (x,y) = (x,y)
moves [d] (x,y) = move d (x,y)
moves (d:ds) (x,y) =
    let (xNew,yNew) = move d (x,y) in moves ds (xNew,yNew)
-- replace with actual definition of moves, and likewise for the
-- other 'undefined' functions

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ(add x y)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ Zero) x = x
mult (Succ y) x = add (mult y x) x -- in mult method y-1 equals (y-1)*x + x

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) =  (nat2int x) + 1 -- test case remember add() eg.(Succ (Succ (Succ Zero)))

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ(int2nat(x-1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

-- n is the integer, tVaule is the value of node, left is the left part of tree t, and right is the right part of tree t.
insert :: Int -> Tree -> Tree
insert n Leaf = Node n Leaf Leaf
insert n (Node tValue left right)
    | n > tValue = Node tValue left (insert n right)
    | n < tValue = Node tValue (insert n left) right
    | n == tValue = Node tValue left right
-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Eq, Show, Read, Ord)

pinsert :: (Ord type') => type' -> PTree type'-> PTree type'
--pinsert :: FIXME  -- uncomment and replace with the proper type of pinsert
pinsert value PLeaf = PNode value PLeaf PLeaf
pinsert value (PNode nodeValue pLeft pRight) 
    | value > nodeValue = PNode nodeValue pLeft (pinsert value pRight)
    | value < nodeValue = PNode nodeValue (pinsert value pLeft) pRight
    | value == nodeValue = PNode nodeValue pLeft pRight