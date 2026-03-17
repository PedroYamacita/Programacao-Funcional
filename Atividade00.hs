import Text.Printf

addA :: Int -> Int
addA a = a + 1

main :: IO ()
main = do
   
    inputA <- getLine
    
    let a = read inputA :: Int
    print (addA a)