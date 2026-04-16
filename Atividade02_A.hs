somaDivisores :: Int -> Int
somaDivisores n = sum[x | x <- [1..n-1], n `mod` x == 0]

classificar :: Int -> Int
classificar n
    | soma == n = 1
    | soma > n = 2 
    | otherwise = 0
    where soma = somaDivisores n
    
main :: IO()
main = do
    line1 <- getLine
    line2 <- getLine
    
    let a = read line1 :: Int
    let b = read line2 :: Int
    
    let intervalo = [min a b .. max a b]
    let resultados = map classificar intervalo
    
    print $ length (filter (==0) resultados) 
    print $ length (filter (==1) resultados) 
    print $ length (filter (==2) resultados) 