rendaPorSecao :: (Float, Float, Float) -> Float
rendaPorSecao (valor_ticket, cadeiras_secao, valor_secao) = ((valor_ticket * cadeiras_secao) - valor_secao)

numeroSecoes :: (Float, Float, Float, Float, Float) -> Float
numeroSecoes(ticket, assentos, secao, investimentoFinanceiro, retornoFinanceiro)
    | retornoFinanceiro <= investimentoFinanceiro = 1.0 + numeroSecoes(ticket, assentos, secao, investimentoFinanceiro,retornoFinanceiro + constante)
    | otherwise = 1.0
        where constante = rendaPorSecao(ticket, assentos, secao)

main = do 
        putStr "custo da Peça: "
        teatro <- getLine
        putStr "custo para cada secao: "
        secao <- getLine
        putStr "valor do ticket: "
        ticket <- getLine
        putStr "total de assentos para cada secao: "
        assentos <- getLine

        putStr "\nDevem ser vendidos "
        putStr (show(numeroSecoes(read ticket :: Float, read assentos :: Float, read secao :: Float,read teatro :: Float, rendaPorSecao(read ticket :: Float, read assentos :: Float, read secao :: Float)) * read assentos :: Float))
        putStr " tickets"
        putStr "\n"
        putStr (show(numeroSecoes(read ticket :: Float, read assentos :: Float, read secao :: Float,read teatro :: Float, rendaPorSecao(read ticket :: Float, read assentos :: Float, read secao :: Float))))
        putStr " Secoes devem ser realizadas para obter lucro"
        putStr "\n"