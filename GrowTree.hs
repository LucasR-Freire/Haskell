-- GrowTree

data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree)=
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node x leftSubtree rightSubtree) =
  x + treeSum leftSubtree + treeSum rightSubtree

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x<maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
-- add a new max element to tree
addNewMax Leaf = Node 0 Leaf Leaf -- input tree with nodes
addNewMax (Node x t1 Leaf) = Node x t1 ( Node (x+1) Leaf Leaf) -- this is the rightmost nodes
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go fown right subtree



treeToList :: Tree -> [Int] -> [Int]
-- convert a Tree into List
treeToList Leaf xs = xs
treeToList (Node x t1 t2) xs =
  do
      --treeToList t2 (treeToList t1 (x:xs))
      treeToList t1 (x:xs)
      treeToList t2 (x:xs)
