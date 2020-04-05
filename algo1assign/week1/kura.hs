import Data.List

lpad m xs = replicate (m - length ys) 0 ++ ys
    where ys = take m xs

digits :: Integer -> [Int]
digits = map (read . return) . show

addr :: [Int] -> [Int] -> (Int, [Int])
addr [] [] = (0, [])
addr (x:xs) (y:ys) = (co, z : z0)
  where (c, z0) = addr xs ys
        total = x + y + c
        co = div total 10
        z = mod total 10

add :: [Int] -> [Int] -> [Int]
add x y = result
  where len = max (length x) (length y)
        (c0, z0) = addr (lpad len x) (lpad len y)
        result = if c0 > 0 then c0 : z0 else z0

subr :: [Int] -> [Int] -> (Int, [Int])
subr [] [] = (0, [])
subr (x:xs) (y:ys) = (co, z : z0)
  where (c, z0) = subr xs ys
        total = x - y - c
        co = if total < 0 then 1 else 0
        z = if total < 0 then total + 10 else total

sub :: [Int] -> [Int] -> [Int]
sub x y = result
  where len = max (length x) (length y)
        (_, result) = subr (lpad len x) (lpad len y)

lsplit :: [Int] -> ([Int], [Int])
lsplit x = (front, back)
  where len = div (length x) 2
        front = (take len x)
        back = (drop len x)

mul1 :: Int -> Int -> [Int]
mul1 x y = z
  where total = x * y
        c0 = div total 10
        z0 = mod total 10
        z = if c0 > 0 then c0 : [z0] else [z0]

mulr :: [Int] -> [Int] -> [Int]
mulr [] [] = []
mulr [x] [y] = mul1 x y
mulr x y = dropWhile(<1) z
  where n0 = max (length x) (length y)
        n = if mod n0 2 == 1 then n0 + 1 else n0
        x0 = lpad n x
        y0 = lpad n y
        (a, b) = lsplit x0
        (c, d) = lsplit y0
        ac = mulr a c
        ad = mulr a d
        bc = mulr b c
        bd = mulr b d
        ac_shift = ac ++ replicate n 0
        ad_shift = ad ++ replicate (div n 2) 0
        bc_shift = bc ++ replicate (div n 2) 0
        z = add (add (add ac_shift ad_shift) bc_shift) bd

kura :: [Int] -> [Int] -> [Int]
kura [] [] = []
kura [x] [y] = mul1 x y
kura x y = dropWhile(<1) z
  where n0 = max (length x) (length y)
        n = if mod n0 2 == 1 then n0 + 1 else n0
        x0 = lpad n x
        y0 = lpad n y
        (a, b) = lsplit x0
        (c, d) = lsplit y0
        ac = kura a c
        bd = kura b d
        abcd = kura (add a b) (add c d)
        ad_bc = sub (sub abcd ac) bd
        ac_shift = ac ++ replicate n 0
        ad_bc_shift = ad_bc ++ replicate (div n 2) 0
        z = add (add ac_shift ad_bc_shift) bd

test :: ([Int] -> [Int] -> [Int]) -> Integer -> Integer -> IO ()
test f a b = do let expected = show (digits (a * b))
                let result = show (f (digits a) (digits b))
                putStr "Expected "
                putStrLn expected
                putStr "Result   "
                putStrLn result
                if expected == result then
                  putStrLn "OK"
                else
                  putStrLn "FAIL"
                putStrLn ""

testwrap :: Integer -> Integer -> IO ()
testwrap a b = do putStrLn "mulr:"
                  test mulr a b
                  putStrLn "kura:"
                  test kura a b

tests :: IO ()
tests = do testwrap 12 34
           testwrap 123 456
           testwrap 1234 5678
           testwrap 123 5678
           testwrap 999 9999
           testwrap 991 191999991
           let x = 3141592653589793238462643383279502884197169399375105820974944592
           let y = 2718281828459045235360287471352662497757247093699959574966967627
           testwrap x y
