module Lecture9 where

main :: IO ()
main = do
    letters "Hello"
    letters' "World"

l :: IO ()
l = getLine >>= letters

letters :: String -> IO ()
letters = mapM_ (putStrLn . (: []))

letters' :: String -> IO ()
letters' = sequence_ . map (putStrLn . (: []))

letters'' :: String -> IO ()
letters'' "" = return ()
letters'' (c : cs) = do
    putStrLn [c]
    letters'' cs

hugorm :: IO ()
hugorm = do
    n <- readLn
    xs <- getNums n
    print $ sum xs

getNums :: Int -> IO [Int]
getNums = sequence . flip replicate readLn

getNums' :: Int -> IO [Int]
getNums' 0 = return []
getNums' n = do
    x <- readLn
    xs <- getNums' (n - 1)
    return (x : xs)

sumInts :: Int -> IO Int
sumInts n = do
    m <- readLn
    if m == 0
        then return n
        else sumInts (n + m)

whileIO :: IO a -> (a -> Bool) -> (a -> b -> b) -> b -> IO b
whileIO io p f b = do
    a <- io
    if p a
        then whileIO io p f (f a b)
        else return b

sumInts' :: Int -> IO Int
sumInts' = whileIO readLn (/= 0) (+)
