newtype ST s a = ST { runST :: s -> (a, s) }

instance Functor (ST s) where
    fmap g (ST st) = ST (\s -> let (x,s') = st s in (g x,s'))

instance Applicative (ST s) where
    pure x = ST (x,)
    (ST stf) <*> (ST stx) = ST (\s ->
        let (f,s') = stf s
            (x,s'') = stx s'
         in (f x,s''))

instance Monad (ST s) where
    (ST st) >>= f = ST (\s ->
        let (x,s') = st s
            (ST st') = f x
         in st' s')

get :: ST s s
get = ST (\s -> (s,s))

put :: s -> ST s ()
put s = ST $ const ((),s)

modify :: (s -> s) -> ST s ()
modify f = do
    s <- get
    put $ f s

newtype W x = W x deriving Show

instance Functor W where
    fmap f (W x) = W (f x)

instance Applicative W where
    pure = W
    W f <*> W x = W (f x)

instance Monad W where
    (W x) >>= f = f x

wrapadd :: (Monad m, Num a) => a -> m a -> m a
wrapadd a b = do
    b' <- b
    return (a + b')

h :: (Monad m, Num a) => m a -> m a -> m a
h a b = do
    a' <- a
    b' <- b
    return (a' + b')

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

minorder :: Ord a => Tree a -> Maybe a
minorder = fmap fst . minorder'

minorder' :: Ord a => Tree a -> Maybe (a, a)
minorder' (Leaf x) = Just (x, x)
minorder' (Node l r) = do
    (lmin, lmax) <- minorder' l
    (rmin, rmax) <- minorder' r
    if lmax > rmin
        then Nothing
        else Just (lmin, rmax)

main :: IO ()
main = do
    print (tuple (Just 1) Nothing :: Maybe (Int, Int))
    print (tuple (Just 1) (Just 2) :: Maybe (Int, Int))
    print (tuple' (Just 1) Nothing :: Maybe (Int, Int))
    print (tuple' (Just 1) (Just 2) :: Maybe (Int, Int))
    print (runST comp 1)
    print $ minorder (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3))
    print $ minorder (Node (Node (Leaf 1) (Leaf 3)) (Leaf 2))
    print $ foldM safeDiv [3,2,1] 60
    foldM (\x y -> dingo (x + y)) [1,2,3,4] 0
    return ()

comp :: ST Int Int
comp = do
    modify (+1)
    modify (*2)
    return 2

tuple :: Monad m => m a -> m b -> m (a, b)
tuple a b = do
    a' <- a
    b' <- b
    return (a', b')

tuple' :: Monad m => m a -> m b -> m (a, b)
tuple' a b = a >>= \a' -> b >>= \b' -> return (a', b')

-- equiv z s f = z >>= \y -> s y >>= \_ -> return (f y)
equiv z s f = z >>= \y -> s y >> return (f y)

foldM :: Monad m => (a -> b -> m b) -> [a] -> b -> m b
foldM _ [] b = return b
foldM f (x:xs) b = f x b >>= foldM f xs

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

dingo :: Int -> IO Int
dingo x = do
    print x
    return x
