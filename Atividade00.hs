import Text.Printf

addA :: Double -> Double
addA a = a + 1

main :: IO ()
main = do
   
    inputA <- getLine
    
    let a = read inputA :: Double
    print (addA a)