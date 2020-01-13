module Lib
    ( BST(..)
    , empty
    , size
    , inOrder
    , postOrder
    , preOrder
    , size
    , insert
    )
where

import           Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- |The data structure representing a binary search tree that can hold any
-- arbitrary data
data BST a = Empty | Node (BST a) a (BST a) deriving (Show, Eq, Ord)

-- |Whether the binary search tree is empty
empty :: BST a -> Bool
empty Empty = True
empty _     = False

-- |Calculate the size of a binary search tree
size :: (Ord a) => BST a -> Int
size Empty                    = 0
size (Node left middle right) = size left + size right + 1

-- |Filters a "Maybe" boolean. If the variable is an instance of `Nothing`,
-- this will return false. Otherwise, it will unwrap the boolean.
filterMaybeBool :: Maybe Bool -> Bool
filterMaybeBool (Just x) = x
filterMaybeBool Nothing  = False

-- |Insert an element into the binary search tree (this does not balance the
-- tree)
insert :: (Ord a, Eq a) => BST a -> a -> BST a
insert Empty new = Node Empty new Empty
insert (Node left value right) new
    | new <= value = Node (insert left new) value right
    | otherwise    = Node left value (insert right new)

-- |Find an element in the tree, if it is present. This method returns the
-- subtree that the element is in.
find :: (Ord a, Eq a) => BST a -> a -> Maybe (BST a)
find Empty _ = Nothing
find middle@(Node left value right) x | x == value = Just middle
                                      | x <= value = find left x
                                      | otherwise  = find right x

-- |Returns whether an element exists in a binary search tree
exists :: (Ord a, Eq a) => BST a -> a -> Bool
exists tree x = isNothing $ find tree x

-- |Returns an in-order traversal of the tree
inOrder :: BST a -> [a]
inOrder Empty                    = []
inOrder (Node left middle right) = inOrder left ++ [middle] ++ inOrder right

-- |Returns a pre-order traversal of the tree
preOrder :: BST a -> [a]
preOrder Empty                    = []
preOrder (Node left middle right) = [middle] ++ preOrder left ++ preOrder right

-- |Returns a post-order traversal or the tree
postOrder :: BST a -> [a]
postOrder Empty = []
postOrder (Node left middle right) =
    postOrder left ++ postOrder right ++ [middle]

-- |Convert a list to a BST
fromList :: (Ord a, Eq a) => [a] -> BST a
fromList = foldl insert Empty
