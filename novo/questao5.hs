import Data.Char

--Letra A Início
verificaIgualdade :: Eq a => (a, [a]) -> Bool
verificaIgualdade(c, []) = True
verificaIgualdade(c, v:t)
 |c == v = False
 |otherwise = verificaIgualdade(c, t)

exclusivo :: Eq a => ([a], [a]) -> [a]
exclusivo ([], v: t) = []
exclusivo(c:r, v: t)
 | verificaIgualdade(c, v:t) = c: exclusivo(r, v: t)
 | otherwise = exclusivo(r, v: t)
--Letra A final

unindoAeB :: ([a], [a]) -> [a]
unindoAeB([], []) = []
unindoAeB([], v: t) = v: unindoAeB([], t)
unindoAeB(c:r, v: t) = c: unindoAeB(r, v: t)


maiorQueCubo :: (Num a, Ord a) => (a, [a], [a]) -> [a]
maiorQueCubo(a, [], []) = []
maiorQueCubo(a, c:r, [])
 | c^2 > a = (c^2): maiorQueCubo(a, r, [])
 | otherwise = maiorQueCubo(a, r, [])

maiorQueCubo(a, [], v: t)
 | v^2 > a = v^2: maiorQueCubo(a, [], t)
 | otherwise = maiorQueCubo(a, [], t)

maiorQueCubo(a, c:r, v: t)
 | (c^2 + v^2)>a = (c^2 + v^2): maiorQueCubo(a, r, t)
 | otherwise = maiorQueCubo(a, r, t)

main2 :: (Show a, Num a, Ord a) => ([a], [a]) -> IO ()
main2(c:r, v: t) = do
    putStr " (A-B) || (B-A) == "
    putStr(show(unindoAeB(exclusivo(c:r, v: t),exclusivo(v: t, c: r))))
    putStr "\n"
    putStr "O cubo dos primeiros elementos é: "
    putStr(show(c^3+v^3))
    putStr "\n"
    putStr(show(maiorQueCubo(c^3+v^3, r, t)))
    putStr "\n"