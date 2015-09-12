-- | Main entry point to the application.
module Main where


-- | 1 Last element of a list
myLast :: [a] -> a
myLast [] = error "Empty List does NOT have a head"
myLast xs = head $ reverse xs

myLast' [] = error "Empty List does NOT have a head"
myLast' [x] = x
myLast' (x:xs) = myLast xs

-- | 2 Last but one element of a list
myButLast :: [a] -> a
myButLast [] = error "Empty List does NOT have a last but one element"
myButLast xs  = head $ drop 1 $ reverse xs

myButLast' [] = error "Empty List does NOT have a last but one element"
myButLast' (x:xs) = myButLastTail xs x
    where
        myButLastTail [_] prev = prev
        myButLastTail (x:xs) prev = myButLastTail xs x

-- | 3 Find the kth element
elementat :: [a] -> Int -> a
elementat (x:xs) 1 = x
elementat (x:xs) k = elementat (xs) (k-1)

-- | 4 Find length of a list
myLength :: [a] -> Int
myLength xs = myLengthTail xs 0
    where
        myLengthTail (x:xs) acc = myLengthTail (xs) (acc + 1)
        myLengthTail [] acc = acc

myLength' :: [a] -> Int
myLength' list = myLength_acc list 0
    where
        myLength_acc [] n = n
        myLength_acc (_:xs) n = myLength_acc xs (n + 1)


-- | 5 Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myRevTail xs [x]
    where
        myRevTail (x:xs) acc = myRevTail xs (x:acc)
        myRevTail [] acc = acc



-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"
   -- putStrLn $ show $ myLast [1..1000000]
   -- putStrLn $ show $ myLast' [1..10]
   -- putStrLn $ show $ myButLast [1..1000000]
   -- putStrLn $ show $ elementat [1..1000000] 20
   -- putStrLn $ show $ myLength' [1..10000]
   -- putStrLn $ show $ myLength [1..10000]
    putStrLn $ show $ myReverse [1..10]
    putStrLn $ show $ myButLast' [1..1000]
