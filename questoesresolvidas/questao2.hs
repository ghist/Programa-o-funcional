import Data.Char
import Data.List
import System.IO (getContents')

-- Função para verificar se uma letra é uma vogal
ehVogal :: Char -> Bool
ehVogal c = elem c "aeiouAEIOU"

-- Função para contar as vogais de uma string
contarVogais :: String -> Int
contarVogais str = length [c | c <- str, ehVogal c]

-- Função para filtrar strings com tamanho >= 4
filtrarTamanho :: [String] -> [String]
filtrarTamanho = filter (\str -> length str >= 4)

-- Função para filtrar strings que começam com vogais
filtrarVogalInicio :: [String] -> [String]
filtrarVogalInicio = filter (\str -> ehVogal (head str))

-- Função para filtrar strings que começam e terminam com vogais
filtrarVogalInicioEFim :: [String] -> [String]
filtrarVogalInicioEFim = filter (\str -> ehVogal (head str) && ehVogal (last str))

-- Função para retornar uma lista de tuplas, cada tupla composta por uma string da lista e o número de vogais da string
listaContarVogais :: [String] -> [(String, Int)]
listaContarVogais strLista = [(str, contarVogais str) | str <- strLista]

-- Função para juntar duas listas e retornar uma lista ordenada com strings com tamanho >= 4
mergeAndSort :: [String] -> [String] -> [String]
mergeAndSort xs ys = sort (filtrarTamanho (xs ++ ys))

-- Função para contar a quantidade de strings que começam com vogais em duas listas
contarVogalInicio :: [String] -> [String] -> Int
contarVogalInicio xs ys = length (filtrarVogalInicio (xs ++ ys))

-- Função para retornar uma lista com strings que começam e terminam com vogais em duas listas
vogalInicioEFimLista :: [String] -> [String] -> [String]
vogalInicioEFimLista xs ys = filtrarVogalInicioEFim (xs ++ ys)

main :: IO ()
main = do

    -- Leitura da primeira lista de strings
    putStrLn "Digite a primeira lista de strings, terminando com uma linha vazia:"
    list1 <- getStrings

    -- Leitura da segunda lista de strings
    putStrLn "Digite a segunda lista de strings, terminando com uma linha vazia:"
    list2 <- getStrings
    

    putStr "lista ordenada contendo strings das duas listas onde as mesmas deve ter tamanho >= a 4: "
    print (mergeAndSort list1 list2)

    putStr "Quantidade de strings das duas listas que iniciam com vogais: "
    print (contarVogalInicio list1 list2)

    putStr "Lista contendo strings das duas listas que iniciam e terminam com vogal: "
    print (vogalInicioEFimLista list1 list2)


-- Função para ler uma lista de strings do usuário
getStrings :: IO [String]
getStrings = do
  str <- getLine
  if str == ""
    then return []
    else do
      strList <- getStrings
      return (str : strList)