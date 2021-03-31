module Main where

import Lib

type Posicao   = (Int ,Int)
data Tabuleiro = Tabuleiro
    { celulas :: [[Char]]
    , largura :: Int
    , altura  :: Int
    } deriving (Show)

valorCelulaViva :: Char
valorCelulaViva = 'v'

valorCelulaMorta :: Char
valorCelulaMorta = 'm'

valorCelulaZumbi :: Char
valorCelulaZumbi = 'z'

posicoesVizinhas :: Tabuleiro -> Posicao -> [Posicao]
posicoesVizinhas Tabuleiro {largura=l,altura=a} (x, y) =
    map (mapeiaPosicao l a) [(x-1,y-1), (x, y-1), (x+1, y-1),
                             (x-1,y) ,{-(x, y),-} (x+1,y)   ,
                             (x-1,y+1), (x, y+1), (x+1, y+1)]

mapeiaPosicao :: Int -> Int -> Posicao -> Posicao
mapeiaPosicao largura altura (x, y) = (mod x largura, mod y altura)

contarElemento :: Eq a => a -> [a] -> Int
contarElemento e [] = 0
contarElemento e (x:xs)
    | e == x    = 1 + contarElemento e xs
    | otherwise = contarElemento e xs

valorCelula :: Tabuleiro -> Posicao -> Char
valorCelula Tabuleiro{celulas=c,largura=l,altura=a} (x, y)
    | x<l && y<a = c !! x !! y
    | otherwise  = ' '

totalVizinhosVivos :: Tabuleiro -> Posicao -> Int
totalVizinhosVivos tabuleiro (x,y) = 
    contarElemento 'v' (map (valorCelula tabuleiro) (posicoesVizinhas tabuleiro (x,y)))

totalVizinhosMortos :: Tabuleiro -> Posicao -> Int
totalVizinhosMortos tabuleiro (x,y) = 
    contarElemento 'm' (map (valorCelula tabuleiro) (posicoesVizinhas tabuleiro (x,y)))

totalVizinhosZumbis :: Tabuleiro -> Posicao -> Int
totalVizinhosZumbis tabuleiro (x,y) = 
    contarElemento 'z' (map (valorCelula tabuleiro) (posicoesVizinhas tabuleiro (x,y)))

geraNovoTabuleiro :: Tabuleiro -> Tabuleiro
geraNovoTabuleiro Tabuleiro {celulas=c,largura=l,altura=a} =
    Tabuleiro {celulas = [['a']],largura=l,altura=a}
    -- substituir por: zumbi < mortos < vivos

iteraTabuleiro :: Tabuleiro -> Tabuleiro -> Int -> Int -> Tabuleiro
iteraTabuleiro tabuleiroAtual tabuleiroNovo i iMaximo
    | i == iMaximo                                    = tabuleiroNovo
    | celulas tabuleiroAtual == celulas tabuleiroNovo = tabuleiroNovo
    | otherwise                                       =
        iteraTabuleiro tabuleiroNovo (geraNovoTabuleiro tabuleiroNovo) (i+1) iMaximo




-- main :: IO ()
-- main = someFunc
-- main = mapM_ process . takeWhile (/= "q") . lines =<< getContents
--   where process line = do -- whatever you like, e.g.
--                           putStrLn line

main = do 
    print (contarElemento 'a' "abracadabra")


{- SECTION - Entrada e Saida
NOTE - pegar o tabuleiro definido pelo usuario
NOTE - pegar o numero maximo de iteracoes desejadas
NOTE - (1) as iteracoes terminam ao chegar no maximo desejado; OU
NOTE - (2) as iteracoes terminam quando não ocorrerem mais alteracoes no tabuleiro
NOTE - apresentar o tabuleiro ao fim das iteracoes
TODO - Entrada e Saida
!SECTION -}
{- SECTION - Regras
NOTE - reproducao     - morta -> viva : =3 celulas vivas adjacentes
NOTE - infeccao       - viva -> zumbi : >0 celulas zumbi adj.
NOTE - subpopulacao   - viva -> morta : <2 celulas vivas adj. e 0 zumbi adj.
NOTE - superpopulacao - viva -> morta : >3 celulas vivas adj. e 0 zumbi adj.
NOTE - inanicao       - zumbi -> morta : =0 celulas vivas adj.
TODO - Regras
!SECTION -}