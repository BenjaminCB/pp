-- Name: Benjamin Clausen Bennetzen
-- AAU mail address: bbenne20@student.aau.dk
-- Study number: 20204861


-- PROBLEM 1

-- 1.1
-- rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

-- the function rotate uses parametric polymorphism as its type has a type variable
-- that does not have have any type class constraints, namely a.

-- 1.2
-- allrotate :: [a] -> [[a]]
allrotate xs = allrotate' (length xs) xs
  where
    allrotate' 0 _ = []
    allrotate' n xs = xs : allrotate' (n - 1) (rotate xs)

-- the function allrotate uses parametric polymorphism as its type has a type variable
-- that does not have have any type class constraints, namely a.

-- 1.3
allrotate' xs = foldr (\x acc -> x : map rotate acc) [] (replicate (length xs) xs)

-- PROBLEM 2

-- 2.1
data Tree a = Node (Tree a) (Maybe a) (Tree a)
            | Leaf a
            deriving (Show)
t1 = Node (Node (Leaf 17) Nothing (Leaf 484000)) Nothing (Leaf 1964)
t2 = Node (Leaf "plip") (Just "bingo") (Node (Leaf "uhu") (Just "plop") (Leaf "fedtmule"))

-- 2.2
-- isfull :: Tree a -> Bool
isfull (Leaf _) = True
isfull (Node _ Nothing _) = False
isfull (Node l (Just _) r) = isfull l && isfull r

-- 2.3
-- preorder :: Tree a -> Maybe [a]
preorder (Leaf a) = return [a]
preorder (Node l v r) = do
    pre1 <- preorder l
    first <- v
    pre2 <- preorder r
    return $ [first] ++ pre1 ++ pre2

-- PROBLEM 3

-- 3.1
-- remove :: (Foldable t, Eq a) => t a -> [a] -> [a]
remove xs ys = [ y | y <- ys, y `notElem` xs ]

-- 3.2
-- remove' :: (Foldable t1, Foldable t2, Eq a) => t1 a -> t2 a -> [a]
remove' xs = foldr (\y acc -> if y `elem` xs then acc else y:acc) []

-- i have chosen to call it remove' in this problem to not have name clashes

-- PROBLEM 4

newtype WrapString a = WS (a,String) deriving Show

instance Functor WrapString where
    fmap f (WS (x,s)) = WS (f x, s)

-- 4.1
instance Applicative WrapString where
    pure a = WS (a, "")
    WS (f, s) <*> WS (x, _) = WS (f x, s)

-- 4.2
instance Monad WrapString where
    return = pure
    WS (x, s) >>= f = let WS (y, _) = f x in WS (y, s)

-- 4.3
pairup ma mb = do
    a <- ma
    b <- mb
    return (a,b)

-- PROBLEM 5

-- 5.1
f x y z = if x < y && y < z then (x+y, y+z) else (z+x, x+y)

-- There is a type variable which means the function uses polymorphism
-- Since all type variables have a type class constraint it only uses ad hoc polymorphism

-- 5.2
g = zip (map toInteger [1..]) (repeat (const 'a'))

-- There is a type variable which means the function uses polymorphism
-- Since no type variables have a type class constraint it only uses parametric polymorphism

-- 5.3
h f x = f x True

-- There is a type variable which means the function uses polymorphism
-- Since no type variables have a type class constraint it only uses parametric polymorphism

-- 5.4
j = [1+1, succ 1]
-- j = [1..]
-- j = map negate $ enumFrom 1

-- hopefully this is correct i have tried various expressions that when typed in ghci directly
-- give the correct answer, but when i type j in ghci i get j :: [Integer]

-- There is a type variable which means the function uses polymorphism
-- Since all type variables have a type class constraint it only uses ad hoc polymorphism

-- PROBLEM 6

-- 6.1
naturals = let f n = n : f (succ n) in f 1

-- 6.2
facs = map (\n -> product [1..n]) [0..]

-- 6.3
facs' = 1 : zipWith (*) [1..] facs'
