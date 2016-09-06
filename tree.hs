main :: IO ()
main = print "poo"

data Tree a = Nil | Node (Tree a) a (Tree a)
  deriving (Eq, Ord, Show)

-- leaf :: (Ord a) => a -> Tree a
-- leaf x = Node Nil x Nil

empty :: (Ord a) => Tree a -> Bool
empty Nil = True
empty   _ = False

contains :: (Ord a) => Tree a -> a -> Bool
contains Nil _ = False
contains (Node t1 v t2) x
        | x == v = True
        | x < v = contains t1 x
        | x > v = contains t2 x

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node t1 v t2) x
        | x == v = Node t1 v t2
        | x < v = insert t1 x
        | x > v = insert t2 x
