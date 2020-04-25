import System.IO
import Data.List (sort)

randInt :: Int -> Int
randInt seed = (7 * seed + 12345) `mod` (2^32)

type Edge = (Int, Int)

-- Main Algorithm Start
getRandomEdge :: Int -> [Edge] -> Edge
getRandomEdge rand edges = edges !! (rand `mod` (length edges))

replaceEdges :: Edge -> [Edge] -> [Edge]
replaceEdges (u, v) es = [(r u', r v') | (u', v') <- es]
  where
    r val
        | u == val = v
        | otherwise = val

removeSelfLoops :: [Edge] -> [Edge]
removeSelfLoops e = [(u,v) | (u,v) <- e, u /= v]

algo :: Int -> [Edge] -> [Edge]
algo rand edges = e2
  where
    e0 = getRandomEdge rand edges
    e1 = replaceEdges e0 edges
    e2 = removeSelfLoops e1

algoloop :: Int -> Int -> [Edge] -> [Edge]
algoloop 2 _ edges = edges
algoloop vcnt rand e = algoloop (vcnt-1) (randInt rand) (algo rand e)

-- Main Algorithm End

-- Using this function for partial application to utilize map in main
algoloopwrap :: Int -> [Edge] -> Int -> Int
algoloopwrap vcnt edges randnum = numEdges $ algoloop vcnt randnum edges

main :: IO ()
main = do
    edges <- graphFromFile "kargerMinCut.txt"
    let vcnt = virtexCount edges
    let alwrap = algoloopwrap vcnt edges
    -- 1000 attempts in ~8s when compiling with: ghc -O2 karger.hs
    let cuts = map alwrap [1..1000]
    putStrLn $ show cuts
    putStrLn $ "max cut: " ++ (show (maximum cuts))
    putStrLn $ "min cut: " ++ (show (minimum cuts))

-- Below this line, Slow is OK, as they are not called in the "algoloop"

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

numEdges :: [Edge] -> Int
numEdges e = length e

virtexCount :: [Edge] -> Int
virtexCount e = length . uniq . sort $ concatMap (\(a,b) -> [a,b]) e  

addEdge :: Edge -> [Edge] -> [Edge]
addEdge e@(u,v) edges
    | e' `elem` edges = edges
    | otherwise       = (e:edges)
  where
    e' = (v,u)

addEdges :: [Edge] -> [Edge] -> [Edge]
addEdges es edges = foldr addEdge edges es

graphFromFile :: String -> IO [Edge]
graphFromFile filename = do
    handle <- openFile filename ReadMode  
    edges <- gfLoop [] handle
    hClose handle
    return edges
  where
    gfLoop :: [Edge] -> Handle -> IO [Edge]
    gfLoop g inhandle = do
        ineof <- hIsEOF inhandle
        if ineof then
            return g
        else do
            inpStr <- hGetLine inhandle
            let (u:vlst) = map toInt $ words inpStr
            let gmod = addEdges (zip [u,u..] vlst) g
            gfLoop gmod inhandle
          where
            toInt :: String -> Int
            toInt s = read s :: Int
