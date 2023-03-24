import System.Random (randomRIO)

cadeira(x)
 |(mod x 3) == 0 = 2
 |(mod x 3) == 1 = 1
 |(mod x 3) == 2 = 0

main2(x,y) = do
    x <- randomRIO (1,100::Int)
    y <- randomRIO (1,100::Int)
    putStr "O número que Ana sorteou foi: "
    putStr (show(x))
    putStr "\n"
    putStr "A cadeira que Ana ira sentar é a cadeira: "
    putStr (show(cadeira(x)))
    putStr "\n"
    putStr "O número que Beatriz sorteou foi: "
    putStr (show(y))
    putStr "\n"
    putStr "A cadeira que Beatriz ira sentar é a cadeira: "
    putStr (show(cadeirabeatriz(cadeira(x),cadeira(y))))
    putStr "\n"
    putStr "A cadeira restante para Carolina foi a cadeira: "
    putStr (show(cadeiracarolina(cadeira(x), (cadeirabeatriz(cadeira(x),cadeira(y))))))
    putStr "\n"



cadeirabeatriz(a,x)
 |(a /= x) = x
 |(a == 1) = 0
 |(a == 0) = 2
 |otherwise = 1
cadeiracarolina(a,b)
 |(a == 0 || a== 1) && (b == 0 || b == 1) = 2
 |(a == 1 || a== 2) && (b == 1 || b == 2) = 0
 |otherwise = 1

    