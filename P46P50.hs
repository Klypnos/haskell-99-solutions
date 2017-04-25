module P46P50 where
import Control.Monad
import Data.List
import Data.Ord
import Prelude
--p46 :((
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' a b
    | a /= b = True
    | otherwise = False

not' :: Bool -> Bool
not' True = False
not' False = True

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

xor' :: Bool -> Bool -> Bool
xor' a b = ((not' a) `and'` b) `or'` (a `and'` (not' b))

impl' :: Bool -> Bool -> Bool
impl' a b =  not a `or'` b

equ' :: Bool -> Bool -> Bool
equ' a b = a == b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [ show a ++ " " ++ show b ++ " " ++ show (f a b) | a <-[True,False] , b <-[True,False] ]

--p47 :((
infixl 4 `or'`
infixl 6 `and'`

--p48 :(((((((((((((((((((((((((((((((((
tablen :: Int -> ([Bool]-> Bool) -> IO ()
tablen n f = mapM_ putStrLn [ toStr a ++ show (f a) | a <- (replicateM n [True,False])  ]
    where toStr = unwords. map (\x -> show x ++ space x)
          space True = "  "
          space False = " "

--p49 :((((((((((((((((
gray :: (Eq a, Num a) => a -> [[Char]]
gray 0 = [""]
gray n =  [ a | a <- (map ('0':) (gray (n-1))) ++ (map ('1':) (reverse (gray (n-1)))) ]

--p50 :(((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((
data HTree a = Leaf a | Branch (HTree a) (HTree a)
    deriving (Show)

huffman :: (Ord a, Ord w, Num w) =>  [(a,w)] -> [(a,[Char])]
huffman freq                          = sortBy (comparing fst) $ serialize $ htree $ sortBy (comparing fst) $ [ (w,Leaf a) | (a,w) <- freq]
    where htree [(_,t)]               = t
          htree ((w1,t1):(w2,t2):wts) = htree $ insertBy (comparing fst) (w1+w2, Branch t1 t2) wts
          serialize (Branch l r)      = [ (x,'0':code) | (x,code) <- serialize l ] ++ [ (x,'1':code) | (x,code) <- serialize r ]
          serialize (Leaf x)          = [(x,"")]
