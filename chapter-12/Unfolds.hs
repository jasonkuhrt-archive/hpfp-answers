module Unfolds where




-- 1

-- Examples

-- (take 10 $ iterate (+1) 0) == [0,1,2,3,4,5,6,7,8,9]

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)



-- 2

-- The first argument to unfold `f` must return a type of Maybe Tuple.
--
-- The first Tuple value represents the next value to add to the
-- unfolding list.
--
-- The second Tuple value represents the value to use to compute
-- the next unfold step.
--
-- If Nothing is returned then the unfolding must stop.

-- Examples

-- [0,1,2,3,4] == (take 5 $ unfoldr (\b -> Just (b, b+1)) 0)
-- [0,1,2,3,4] == unfoldr (\b -> if b == 5 then Nothing else Just (b, b+1)) 0

unfoldr' :: (a -> Maybe (b, a)) -> a -> [b]
unfoldr' f now = case f now of
  Nothing              -> []
  Just (result, next)  -> result : unfoldr' f next



-- 3

iterate'' :: (a -> a) -> a -> [a]
iterate'' f = unfoldr' (\x -> Just (x, f x))



-- 4

-- Working with data of type BinaryTree



data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)

  deriving (Show, Ord, Eq)



-- Insert a value into a BinaryTree.

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' a Leaf = Node Leaf a Leaf
insert' a tree@(Node left b right)
  | a == b = tree
  | a  < b = Node (insert' a left) b right
  | a  > b = Node left b (insert' a right)



-- Unfold a BinaryTree.

-- TODO According to book there is a way to implement treeUnfold in terms of
--      `insert'` but so far I have not been able to figure out exactly how.

treeUnfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
treeUnfold f now = case f now of
  Nothing        -> Leaf
  Just (l, x, r) -> Node (treeUnfold f l) x (treeUnfold f r)



-- Build up a BinaryTree of integers.

-- Examples

-- treeBuild 0 == Leaf
-- treeBuild 1 == Node Leaf 0 Leaf
-- treeBuild 2 == Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)
-- treeBuild 3 == Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)) 0 (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = treeUnfold (\x -> if x == n then Nothing else Just (x+1, x, x+1)) 0
