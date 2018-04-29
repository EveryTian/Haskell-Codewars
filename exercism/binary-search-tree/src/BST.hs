module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Nil | BST (BST a) a (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Nil = Nothing
bstLeft (BST l _ _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Nil = Nothing
bstRight (BST _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Nil = Nothing
bstValue (BST _ e _) = Just e

empty :: BST a
empty = Nil

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) Nil
-- fromList = foldr insert Nil . reverse

insert :: Ord a => a -> BST a -> BST a
insert x Nil = BST Nil x Nil
insert x (BST l e r)
    | x <= e = BST (insert x l) e r
    | otherwise = BST l e (insert x r)

singleton :: a -> BST a
singleton x = BST Nil x Nil

toList :: BST a -> [a]
toList Nil = []
toList (BST l e r) = toList l ++ [e] ++ toList r
