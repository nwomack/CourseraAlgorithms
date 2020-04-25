
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (a:as) (b:bs)
        | a <= b    = a : merge as (b:bs)
        | otherwise = b : merge (a:as) bs

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort x = merge left right
    where mid = div (length x) 2
          left = mergesort $ take mid x
          right = mergesort $ drop mid x

test :: IO ()
test = do putStrLn . show . merge [1,3] $ [2,4]
          putStrLn . show . mergesort $ [1,3,2,4]
          let x = [1, 4, 8, 5, 6, 4, 1, 2, 5, 6, 14, 12, 11, 21, 0, 6, 7, 10]
          putStrLn . show . mergesort $ x
