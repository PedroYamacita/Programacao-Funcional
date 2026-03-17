import Text.Printf

isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c = (a + b >= c) && (a + c >= b) && (b + c >= a)

calculateArea :: Double -> Double -> Double -> Double
calculateArea a b c = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2

main :: IO ()
main = do
   
    inputA <- getLine
    inputB <- getLine
    inputC <- getLine
    
    let a = read inputA :: Double
    let b = read inputB :: Double
    let c = read inputC :: Double
    
    if isTriangle a b c
        then print (calculateArea a b c)
        else putStrLn "-"