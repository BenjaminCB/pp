module Lecture7 where

data Nat = Zero | Succ Nat
    deriving (Eq, Ord, Show, Read)

data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving (Eq, Ord, Show, Read)

data Tree2 a
    = L a
    | Empty
    | N (Tree2 a) a (Tree2 a)
    deriving (Eq, Ord, Show, Read)

data Truthtable
    = Val Bool
    | Ass Truthtable String Truthtable

data Aexp
    = Num Int
    | Var String
    | Add Aexp Aexp
    | Mul Aexp Aexp
    deriving (Eq, Ord, Show, Read)

data Dir = Dir String [(String, Int)] [Dir]
    deriving (Eq, Ord, Show, Read)

data Prop
    = Const Bool
    | V String
    | Not Prop
    | And Prop Prop
    | Or Prop Prop

class InVector a where
    (&&&) :: a -> a -> a
    (***) :: a -> a -> Int

instance InVector Bool where
    (&&&) = (&&)
    b *** b' = fromEnum b + fromEnum b'

main :: IO ()
main = do
    print $ nat2int (Succ (Succ (Succ Zero)))
    print $ nat2int (Succ (Succ (Succ (Succ Zero))))
    print $ least (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

least :: (Ord a) => Tree a -> a
least (Leaf x) = x
least (Node l x r) = minimum [least l, x, least r]

eval :: Aexp -> [(String, Int)] -> Maybe Int
eval (Num n) _ = Just n
eval (Var x) env = lookup x env
eval (Add e1 e2) env = do
    n <- eval e1 env
    m <- eval e2 env
    return $ n + m
eval (Mul e1 e2) env = do
    n <- eval e1 env
    m <- eval e2 env
    return $ n * m

insert :: (Ord a) => a -> Tree2 a -> Tree2 a
insert x Empty = L x
insert x (L y)
    | x <= y = N (L x) y Empty
    | otherwise = N Empty y (L x)
insert x (N l y r)
    | x <= y = N (insert x l) y r
    | otherwise = N l y (insert x r)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node l _ r) = 1 + max (height l) (height r)

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l _ r) = abs (height l - height r) <= 1 && balanced l && balanced r
