import Data.List ( sortBy )
import Data.Char ( toLower )
import Data.Ord ( comparing )

type Nome = String
type Estado = String
type Pais = String
type AnoFundacao = Int

data Time = Time {
    nome :: Nome,
    estado :: Estado,
    pais :: Pais,
    anoFundacao :: AnoFundacao
} deriving (Show, Eq, Ord, Read)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let menores = quicksort [y | y <- xs, y < x]
        maiores = quicksort [y | y <- xs, y >= x]
    in menores ++ [x] ++ maiores

ordenarPorNome :: [Time] -> [Time]
ordenarPorNome = quicksort . sortBy (comparing nome)

buscarPorNome :: [Time] -> Nome -> [Time]
buscarPorNome times nomeBusca =
  filter ((== map toLower nomeBusca) . map toLower . nome) times

main :: IO ()
main = do
    putStrLn "Digite o número de times:"
    n <- getLine
    let numTimes = read n :: Int
    lista <- getTimes numTimes
    let listaOrdenada = ordenarPorNome lista
    putStrLn "Lista de times ordenada por nome:"
    mapM_ (putStrLn . nome) listaOrdenada
    putStrLn "Digite o nome do clube que deseja buscar:"
    nome <- getLine
    let resultados = buscarPorNome lista nome
    putStrLn "Resultados da busca:"
    mapM_ print resultados

getTimes :: Int -> IO [Time]
getTimes n
    | n <= 0 = return []
    | otherwise = do
        putStrLn "Digite o nome do time:"
        nome <- getLine
        putStrLn "Digite o estado do time:"
        estado <- getLine
        putStrLn "Digite o país do time:"
        pais <- getLine
        putStrLn "Digite o ano de fundação do time:"
        anoStr <- getLine
        let ano = read anoStr :: Int
        resto <- getTimes (n-1)
        return $ (Time nome estado pais ano) : resto
