import Data.Char
import Data.List



contarVogais :: String -> Int
contarVogais str = length [x | x <- str, ele x "aeiouAEIOU"]


tuplaStrVogais :: [String] -> [(String,Int)]
tuplaStrVogais strList = [(str, contarVogais str) | str <- strList]

maiorQue5IniciaVogal :: [String] -> [String]
maiorQue5IniciaVogal strList = [str | str <- strList, length str > 5, elem (str !! 0) "aeiouAEIOU"]

substituiVogal :: [String] -> [String]
substituiVogal strList = [map(\c -> if elem c "aeiouAEIOU" then '@' else c) str | str <- strList]

iniciaVogal :: [String] -> Int
iniciaVogal strList = length [str | str <- strList, elem (head str) "aeiouAEIOU"]

mergeAndSort :: [String] -> [String]
mergeAndSort strList = sort [str | str <- strList, length str >= 4]

iniciaVogalETermina :: [String] -> [String]
iniciaVogalETermina strList = sort [str | str <- strList, elem(head str)"aeiouAEIOU" && elem(toLower(last str))"aeiou"]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort(x:xs) = quicksort[y | y <- xs, y <= x] ++ [x] ++ quicksort[y | y <- xs, y > x]

maior50Multi3 :: [Int] -> [Int]
maior50Multi3 lista = quicksort [x | x <- lista, odd x, x > 50, mod x 3 == 0]

multiplo3e7Produto :: [Int] -> Int
multiplo3e7Produto lista = product [x | x <- lista, x > 50, mod x 3 == 0 || x < 200, mod x 7 == 0]

paresMaioresQue50 :: [Int] -> [Int]
paresMaioresQue50 lista = [x | (x, i) <- zip lista [0..], i `mod` 2 == 0, x > 50]

imparesMenoresQue200 :: [Int] -> [Int]
imparesMenoresQue200 lista = [x | (x, i) <- zip lista [0..], i `mod` 2 /= 0, x < 200]

maior50Multi31 :: [Int] -> [Int]
maior50Multi31 lista = quicksort [x | x <- lista, odd x, x > 50, mod x 3 == 0]

multiplo3e7Produto1 :: [Int] -> Int
multiplo3e7Produto1 lista = product [x | x <- lista, x > 50, mod x 3 == 0 || x < 200, mod x 7 == 0]


type Nome = String
type Estado = String
type Pais = String
type AnoFundacao = Int

data Time = Time {
    nome :: Nome,
    estado :: Estado,
    pais :: Pais,
    ano :: AnoFundacao
} deriving(Show, Eq, Ord, Read)

quicksort1 :: Ord a => [a] -> [a]
quicksort1 [] = []
quicksort1 (x:xs) = 
    let menores = [y | y <- xs, y < x]
        maiores = [y | y <- xs, y >= x]
    in menores ++ [x] ++ maiores

ordenarPorNome1 :: [Time] -> [Time]
ordenarPorNome1 = quicksort1 . sortBy (comparing nome)

buscarPorNome1 :: [Time] -> Nome -> [Time]
buscarPorNome1 times nomeBusca = 
    filter ((== map toLower nomeBusca) . map toLower . nome) times