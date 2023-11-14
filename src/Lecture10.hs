module Lecture10 where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe

data Onion a = Core a | Layer (Onion a) deriving Show

instance Functor Onion where
  fmap f (Core a) = Core (f a)
  fmap f (Layer a) = Layer (fmap f a)

data UTree a = Node a [UTree a] deriving Show

instance Functor UTree where
  fmap f (Node a []) = Node (f a) []
  fmap f (Node a xs) = Node (f a) (fmap f <$> xs)

-- commented due to duplicate instance
-- instance Functor ((->) a) where
--   fmap f g = f . g

-- commented due to duplicate instance
-- instance Applicative [] where
--     pure a = [a]
--     (f:fs) <*> xs = (f <$> xs) ++ (fs <*> xs)
--     _ <*> _ = []

data Exp a = Var a
           | Val Integer
           | Add (Exp a) (Exp a)
           | Mul (Exp a) (Exp a)
           deriving Show

instance Functor Exp where
    fmap f (Var a) = Var (f a)
    fmap _ (Val a) = Val a
    fmap f (Add a b) = Add (f <$> a) (f <$> b)
    fmap f (Mul a b) = Mul (f <$> a) (f <$> b)

instance Applicative Exp where
    pure = Var

    _ <*> (Val a) = Val a

    (Var f) <*> (Var a) = Var (f a)
    (Var f) <*> (Add a b) = Add (f <$> a) (f <$> b)
    (Var f) <*> (Mul a b) = Mul (f <$> a) (f <$> b)

    (Add f g) <*> (Var a) = Add (f <*> Var a) (g <*> Var a)
    (Add f g) <*> (Add a b) = Add (f <*> a) (g <*> b)
    (Add f g) <*> (Mul a b) = Mul (f <*> a) (g <*> b)

    (Mul f g) <*> (Var a) = Mul (f <*> Var a) (g <*> Var a)
    (Mul f g) <*> (Add a b) = Add (f <*> a) (g <*> b)
    (Mul f g) <*> (Mul a b) = Mul (f <*> a) (g <*> b)

main :: IO ()
main = do
    print $ prodthree [1,2,3] [4,5,6] [7,8,9]
    print $ runReader (runMaybeT (eval' (Add (Val 1) (Var "x")))) [("x", 2)]
    print $ runReader (runMaybeT (eval' (Add (Val 1) (Var "x")))) [("y", 2)]


prodthree :: (Num a) => [a] -> [a] -> [a] -> [a]
prodthree xs ys zs = (\a b c -> a * b * c) <$> xs <*> ys <*> zs

eval :: Exp String -> [(String, Integer)] -> Integer
eval (Var x) env = case lookup x env of
    Just a -> a
    Nothing -> error "Variable not found"
eval (Val x) _ = x
eval (Add x y) env = eval x env + eval y env
eval (Mul x y) env = eval x env * eval y env

eval' :: Exp String -> MaybeT (Reader [(String, Integer)]) Integer
eval' (Var x) = do
    env <- ask
    case lookup x env of
        Just a -> return a
        Nothing -> fail "Variable not found"
eval' (Val x) = return x
eval' (Add x y) = do
    a <- eval' x
    b <- eval' y
    return $ a + b
eval' (Mul x y) = do
    a <- eval' x
    b <- eval' y
    return $ a * b
