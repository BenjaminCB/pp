module TMorris where

main :: IO ()
main = putStrLn "Hello, World!"

class Fluffy f where
    furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
    furry _ [] = []
    furry f (x : xs) = f x : furry f xs

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
    furry _ Nothing = Nothing
    furry f (Just x) = Just $ f x

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
    furry f g = f . g

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
    furry f (EitherLeft (Left a)) = EitherLeft . Left $ f a
    furry _ (EitherLeft (Right b)) = EitherLeft $ Right b

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
    furry _ (EitherRight (Left a)) = EitherRight $ Left a
    furry f (EitherRight (Right b)) = EitherRight . Right $ f b

class Misty m where
    banana :: (a -> m b) -> m a -> m b
    unicorn :: a -> m a

    -- Exercise 6
    -- Relative Difficulty: 3
    -- (use banana and/or unicorn)
    furry' :: (a -> b) -> m a -> m b
    furry' f = banana $ unicorn . f

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
    banana f [] = []
    banana f (x : xs) = f x ++ banana f xs
    unicorn = (: [])

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
    banana _ Nothing = Nothing
    banana f (Just x) = f x
    unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
    banana f g t = f (g t) t
    unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
    banana f (EitherLeft (Left a)) = f a
    banana _ (EitherLeft (Right b)) = EitherLeft $ Right b
    unicorn = EitherLeft . Left

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
    banana _ (EitherRight (Left a)) = EitherRight $ Left a
    banana f (EitherRight (Right b)) = f b
    unicorn = EitherRight . Right

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
-- class Misty m where
--   banana :: (a -> m b) -> m a -> m b
--   unicorn :: a -> m a
--   -- Exercise 6
--   -- Relative Difficulty: 3
--   -- (use banana and/or unicorn)
--   furry' :: (a -> b) -> m a -> m b
--   furry' f = banana $ unicorn . f
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma mf = (\a -> (\f -> unicorn $ f a) `banana` mf) `banana` ma

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] _ = unicorn []
moppy (mx : mxs) f = (\x -> (\xs -> unicorn $ x : xs) `banana` moppy mxs f) `banana` f mx

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = flip moppy id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = mb `apple` (f `furry'` ma)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = mc `apple` banana2 f ma mb

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = md `apple` banana3 f ma mb mc

newtype State s a = State {state :: s -> (s, a)}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
    furry f (State g) = State $ fmap f . g

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
    banana f (State g) = State $ \s ->
        let (_, a) = g s
            (State g') = f a
         in g' s
    unicorn x = State (,x)
