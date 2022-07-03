data BinaryTree a = Leaf a | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

instance Functor BinaryTree where
  fmap f (Leaf l) = Leaf (f l)
  fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

invertBinaryTree :: BinaryTree a -> BinaryTree a
invertBinaryTree (Leaf l) = Leaf l
invertBinaryTree (Node val left right) = Node val (invertBinaryTree right) (invertBinaryTree left)

reduceBinaryTree :: (a -> a -> a) -> BinaryTree a -> a
reduceBinaryTree f (Leaf l) = l
reduceBinaryTree f (Node a left right) = foldr f a [reduceBinaryTree f left, reduceBinaryTree f right]

sortBinaryTree :: (Ord a) => BinaryTree a -> BinaryTree a
sortBinaryTree (Node val left@(Node leftVal _ _) right@(Leaf rightVal)) = if leftVal <= rightVal then (Node val (sortBinaryTree left) right) else (Node val right (sortBinaryTree left))
sortBinaryTree (Node val left@(Leaf leftVal) right@(Node rightVal _ _)) = if leftVal <= rightVal then (Node val left (sortBinaryTree right)) else (Node val (sortBinaryTree right) left)
sortBinaryTree (Node val left@(Leaf leftVal) right@(Leaf rightVal)) = if leftVal <= rightVal then (Node val left right) else (Node val right left)
sortBinaryTree (Node val left@(Node leftVal _ _) right@(Node rightVal _ _)) = if leftVal <= rightVal then Node val (sortBinaryTree left) (sortBinaryTree right) else Node val (sortBinaryTree right) (sortBinaryTree left) 