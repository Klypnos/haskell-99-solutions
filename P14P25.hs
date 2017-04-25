module P14P25 where
import Data.List
import System.Random
--p14
dupli :: [a] -> [a]
dupli [] = []
dupli xs = concatMap (\a -> replicate 2 a) xs

--p15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli xs b = concatMap (\a -> replicate b a) xs

--p16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = dropEvery' ( filter (\a -> a `mod` n /= n-1) [0 .. length xs - 1])
    where
        dropEvery' ys = map (xs !!) ys
--p17
split :: [a] -> Int -> ([a],[a])
split xs a = (take a xs,drop a xs)

--p18
slice :: [a] -> Int -> Int -> [a]
slice xs a b = take (b-a+1) (drop (a-1) xs)

--p19
rotate :: [a] -> Int -> [a]
rotate xs a = take (length xs) . drop (length xs + a) . cycle $ xs

--p20
removeAt :: [a] -> Int -> (a,[a])
removeAt xs a = (xs !! (a-1), take (a-1) xs ++ drop a xs )

--p21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs a | a > 0 && a < length xs =  fst splitted ++ [x] ++ snd splitted
    where
        splitted = splitAt (a-1) xs

--p22
range :: Int -> Int -> [Int]
range a b = [a .. b]


--p23 :(
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs a = do
    g <- getStdGen
    return $ take a [ xs !! i | i <- randomRs (0, length xs - 1) g ]

--p24 :( nub eliminates duplicates
rndArr :: Int -> Int -> IO [Int]
rndArr a b =  take a . nub . randomRs (0,b-1) <$> getStdGen

--p25
rndPermu :: [Int] -> IO [Int]
rndPermu xs = rndSelect xs $ length xs
