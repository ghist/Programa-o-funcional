fatorar(num1, num2, num3, fat, cont)
  | num1 == 1 && num2 == 1 && num3 == 1 = espacamento(fat, cont)
  | mod num1 fat == 0 && mod num2 fat == 0 && mod num3 fat == 0 = fatorar(div num1 fat,div num2 fat,div num3 fat, fat, cont+1)
  | mod num1 fat == 0 && mod num2 fat == 0 && mod num3 fat /= 0 = fatorar(div num1 fat,div num2 fat, num3, fat, cont+1)
  | mod num1 fat == 0 && mod num2 fat /= 0 && mod num3 fat == 0 = fatorar(div num1 fat, num2, div num3 fat, fat, cont+1)
  | mod num1 fat /= 0 && mod num2 fat == 0 && mod num3 fat == 0 = fatorar(num1, div num2 fat,div num3 fat, fat, cont+1)
  | mod num1 fat == 0 && mod num2 fat /= 0 && mod num3 fat /= 0 = fatorar(div num1 fat, num2, num3, fat, cont+1)
  | mod num1 fat /= 0 && mod num2 fat == 0 && mod num3 fat /= 0 = fatorar(num1, div num2 fat, num3, fat, cont+1)
  | mod num1 fat /= 0 && mod num2 fat /= 0 && mod num3 fat == 0 = fatorar(num1, num2,div num3 fat, fat, cont+1)
  | mod num1 fat /= 0 && mod num2 fat /= 0 && mod num3 fat /= 0 = do
                                                                    fatorar(num1, num2, num3, fat+1, 0)
                                                                    espacamento(fat, cont)
                                                                    
                                                                    

espacamento(fat, cont)
  | cont > 0 = do
                putStr(show fat)
                putStr " ---> "
                putStrLn(show cont)
  | otherwise = putStr ""


main = do 
        putStr "Informe o primeiro número:"
        num1 <- getLine
        putStr "Informe o segundo número:"
        num2 <- getLine
        putStr "Informe o terceiro número:"
        num3 <- getLine
        fatorar(read num1 :: Int, read num2 :: Int, read num3 :: Int, 2 , 0)
