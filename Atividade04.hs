import Data.List (sort, sortOn)
import Data.Ord (Down(..))
import Text.Read (readMaybe)
import System.IO

-- Tipo de dado para representar uma linha de registro do CSV
data Record = Record {
    country   :: String,
    confirmed :: Int,
    deaths    :: Int,
    recovery  :: Int,
    active    :: Int
} deriving (Show)

-- Função auxiliar para dividir uma String com base em um caractere delimitador (vírgula)
-- Preserva campos vazios
splitBy :: Char -> String -> [String]
splitBy c s = case break (== c) s of
                (w, "")     -> [w]
                (w, (_:s')) -> w : splitBy c s'

-- Transforma uma linha de texto do CSV em um Record (se for válida)
-- Remove quebras de linha ocultas (\r, \n) 
parseLine :: String -> Maybe Record
parseLine line = 
    let cleanLine = filter (`notElem` "\r\n") line
    in case splitBy ',' cleanLine of
        [c, conf, d, r, act] -> 
            case (readMaybe conf, readMaybe d, readMaybe r, readMaybe act) of
                (Just cn, Just dt, Just rc, Just ac) -> Just $ Record c cn dt rc ac
                _ -> Nothing -- Se não puder converter os números (ex: linha de cabeçalho)
        _ -> Nothing

main :: IO ()
main = do
    -- Leitura dos parâmetros n1, n2, n3 e n4 da entrada padrão
    inputLine <- getLine
    let [n1, n2, n3, n4] = map read (words inputLine)
    
    -- Leitura do arquivo "dados.csv"
    content <- readFile "dados.csv"
    let csvLines = lines content
    
    -- Processamento das linhas
    let records = [r | Just r <- map parseLine csvLines]
    
    -- Soma de "Active" de todos os países onde "Confirmed" >= n1
    let res1 = sum [active r | r <- records, confirmed r >= n1]
    print res1
    
    -- Dos n2 países com maior "Active", a soma das "Deaths" dos n3 com menor "Confirmed"
    let topActive = take n2 $ sortOn (Down . active) records
    let bottomConfirmed = take n3 $ sortOn confirmed topActive
    let res2 = sum (map deaths bottomConfirmed)
    print res2
    
    -- Os n4 países com maior "Confirmed", ordenados alfabeticamente pelo nome
    let topConfirmed = take n4 $ sortOn (Down . confirmed) records
    let res3 = sort (map country topConfirmed)
    mapM_ putStrLn res3