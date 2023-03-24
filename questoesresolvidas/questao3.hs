import Data.List

-- Função quicksort para ordenar uma lista de inteiros
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- Função para retornar os elementos de posições pares maiores que 50
paresMaioresQue50 :: [Int] -> [Int]
paresMaioresQue50 lista = [x | (x, i) <- zip lista [0..], i `mod` 2 == 0, x > 50]

-- Função para retornar os elementos de posições ímpares menores que 200
imparesMenoresQue200 :: [Int] -> [Int]
imparesMenoresQue200 lista = [x | (x, i) <- zip lista [0..], i `mod` 2 /= 0, x < 200]

-- Função para obter o produto dos elementos das duas listas de múltiplos de 3 > 50 e múltiplos de 7 < 200
produtoMultiplos :: [Int] -> [Int] -> Int
produtoMultiplos lista1 lista2 = product (multiplos3 ++ multiplos7)
  where multiplos3 = filter (>50) (filter (\x -> 3 == 0) lista1)
        multiplos7 = filter (<200) (filter (\x -> 7 == 0) lista2)

-- Função para obter os elementos maiores que 50, ímpares e múltiplos de 3
elementosMaiores50Multiplos3 :: [Int] -> [Int]
elementosMaiores50Multiplos3 lista1 = filter (>50) (filter odd (filter (\x -> mod x 3 == 0) (lista1)))

main :: IO ()
main = do
    putStrLn "Digite a primeira lista ordenada: "
    input1 <- getLine
    let lista1 = quicksort (map read (words input1) :: [Int])

    putStrLn "Digite a segunda lista ordenada: "
    input2 <- getLine
    let lista2 = quicksort (map read (words input2) :: [Int])

    putStrLn "Elementos das posições pares maiores que 50: "
    print (paresMaioresQue50 lista1 ++ paresMaioresQue50 lista2)

    putStrLn "Elementos das posições impares menores que 200: "
    print (imparesMenoresQue200 lista1 ++ imparesMenoresQue200 lista2)

    putStrLn "Produto dos elementos das duas listas dos múltiplos de 3 > 50 e dos múltiplos de 7 menos do que 200: "
    print (produtoMultiplos lista1 lista2)

    putStrLn "lista ordenada contendo elementos das duas listas que sejam maiores do que 50 e que sejam ímpares múltiplos de 3"
    print (elementosMaiores50Multiplos3 (quicksort (lista1 ++ lista2)))