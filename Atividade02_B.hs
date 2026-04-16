isPrime :: Int -> Bool
isPrime n 
    | n < 2 = False
    | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]
    
diffs :: [Int] -> [Int]
diffs xs = zipWith (\a b -> b - a) xs (drop 1 xs)

main :: IO()
main = do
    lineX <- getLine
    lineY <- getLine
    
    let x = read lineX :: Int
    let y = read lineY :: Int
    
    let primos = [p | p <- [x..y], isPrime p]
    
    let intervalos = diffs primos
    if null intervalos
        then print 0
        else print (maximum intervalos)