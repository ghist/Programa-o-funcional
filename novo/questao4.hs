import Data.Char
--Letra A. Inicio
verificaExistenciaNaLista(a, []) = True
verificaExistenciaNaLista(a, c:r)
 | toLower(a) == toLower(c) = False
 | otherwise = verificaExistenciaNaLista(a, r)

contandoCarateries [] = 0
contandoCarateries(c:r)
 | verificaExistenciaNaLista(c, r) = 1+ contandoCarateries(r)
 | otherwise = contandoCarateries r

contarLista([c:r]: [[]]) = do 
    putStr "A quantidade de caracteries presentes em "
    putStr (c:r)
    putStr "é: "
    putStr (show(contandoCarateries (c:r)))
    putStr "\n"

contarLista([c:r]: v) = do 
    putStr "A quantidade de caracteries presentes em "
    putStr (c:r)
    putStr " é: "
    putStr (show(contandoCarateries (c:r)))
    putStr "\n"
    contarLista v
contarLista ([]) = putStr "Sem palavras aqui"
contarLista ([]: []) = putStr "Sem palavras aqui"
--Letra A. Final

--Letra B. Inicio
evogal(c)
 | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = True
 |otherwise = False
verificaTipo(c)
 |isDigit(c) = "Digito"
 |evogal(c) &&  isLower(c)= "Vogal minuscula"
 |evogal(c) &&  isUpper(c)= "Vogal maiuscula"
 |isLower(c) = "Consoante minuscula"
 |isUpper(c) = "Consoante maiuscula"
 |otherwise = "Caractere especial"
 
devolveTipos [[c:r]] = [verificaTipo c]: []
devolveTipos([c:r]: v) = [verificaTipo c]: devolveTipos v
devolveTipos ([]) = [["Sem palavras aqui"]]
--Letra B. Final

--Letra C. Inicio
contaVogais [] = 0
contaVogais(c:r)
 | evogal(c) = 1+ contaVogais r
 | otherwise = contaVogais r

devolveMaiorString([c:r]: [], aux)
 |contaVogais(c:r) > aux = [c:r]: []
 |otherwise = []
devolveMaiorString([c:r]: v, aux)
 | contaVogais(c:r) > aux = [c:r]: devolveMaiorString(v, contaVogais(c:r))
 | otherwise = devolveMaiorString(v, aux)


devolveMaiorString1 [] = []
---devolveMaiorString1[[c:r]] = c:r
devolveMaiorString1([c:r]: v)
 |contaVogais (c : r) > contaVogais (devolveMaiorString1 v) = c:r
 |otherwise = devolveMaiorString1 v
--Letra C. Final