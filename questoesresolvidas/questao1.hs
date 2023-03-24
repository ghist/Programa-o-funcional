import Data.List
import Data.Char

-- Função para contar o número de vogais em uma string
contarVogais :: String -> Int
contarVogais str = length [c | c <- str, elem c "aeiouAEIOU"]

-- Função para retornar uma lista de tuplas, onde cada tupla é composta por uma string da lista dada e o número de vogais da string
listaContarVogais :: [String] -> [(String, Int)]
listaContarVogais strList = [(str, contarVogais str) | str <- strList]

-- Função para retornar uma lista de strings que tenham tamanho maior do que 5 e iniciem com vogais
listaLongaVogaisStrings :: [String] -> [String]
listaLongaVogaisStrings strList = [str | str <- strList, length str > 5, elem (toLower (str !! 0)) "aeiou"]

-- Função para retornar uma lista de strings substituindo as vogais em cada string por @
refazerVogais :: [String] -> [String]
refazerVogais strList = [map (\c -> if elem (toLower c) "aeiou" then '@' else c) str | str <- strList]

-- Função principal que lê a lista de strings e retorna as listas resultantes das funções acima
main :: IO ()
main = do
  putStrLn "Informe a lista de strings (um por linha, terminada com uma linha vazia):"
  strList <- getStrings
  putStrLn "Lista de tuplas (string, número de vogais):"
  print (listaContarVogais strList)
  putStrLn "Lista de strings com tamanho maior do que 5 e que iniciam com vogais:"
  print (listaLongaVogaisStrings strList)
  putStrLn "Lista de strings com vogais substituídas por @:"
  print (refazerVogais strList)

-- Função para ler uma lista de strings do usuário
getStrings :: IO [String]
getStrings = do
  str <- getLine
  if str == ""
    then return []
    else do
      strList <- getStrings
      return (str : strList)

-- Esse é um programa em Haskell que lê uma lista de strings do usuário e retorna várias listas resultantes de funções de processamento de strings.

-- A função contarVogais conta o número de vogais em uma string. Isso é feito percorrendo cada caractere da string e verificando se ele está presente na string "aeiouAEIOU".

-- A função listaContarVogais retorna uma lista de tuplas, onde cada tupla é composta por uma string da lista de entrada e o número de vogais da string. Isso é feito usando uma list comprehension para gerar a lista de tuplas.

-- A função listaLongaVogaisStrings retorna uma lista de strings que têm tamanho maior que 5 e iniciam com vogais. Isso é feito usando uma list comprehension que filtra a lista de strings com base nas condições mencionadas.

-- A função refazerVogais retorna uma lista de strings com vogais substituídas por @. Isso é feito usando uma list comprehension para gerar a lista resultante e a função map para aplicar a substituição a cada string.

-- A função main é a função principal que lê a lista de strings do usuário e chama as funções de processamento para retornar as listas resultantes.

-- A função getStrings lê uma lista de strings do usuário, permitindo que o usuário entre com uma string por linha e encerrando a entrada com uma linha vazia.