module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | Cons a (LinkedList a)
    deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Cons h _) = h

fromList :: [a] -> LinkedList a
-- fromList [] = Nil
-- fromList (x:xs) = Cons x $ fromList xs
fromList = foldr Cons Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: LinkedList a -> LinkedList a
next (Cons _ t) = t

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = reverseLinkedList' Nil
    where reverseLinkedList' x Nil = x
          reverseLinkedList' x (Cons y0 y1) = reverseLinkedList' (Cons y0 x) y1

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons h t) = h : toList t
