module Main where

import Lib
import Debug.Trace
import qualified Data.List (delete, nub)

type Posicao   = (Int ,Int)
data Tabuleiro = Tabuleiro
    { celulas :: [[Char]]
    , largura :: Int
    , altura  :: Int
    } deriving (Show)

valorCelulaViva :: Char
valorCelulaViva = 'v'

valorCelulaMorta :: Char
valorCelulaMorta = ' '

valorCelulaZumbi :: Char
valorCelulaZumbi = 'z'

valoresValidos :: [Char]
valoresValidos = [valorCelulaViva,valorCelulaMorta,valorCelulaZumbi]

posicoesVizinhas :: Tabuleiro -> Posicao -> [Posicao]
posicoesVizinhas Tabuleiro {largura=l,altura=a} (x, y) =
    -- delete((x,y), listaSemRepeticao(listaVizinhos))
    Data.List.delete (x,y) $ Data.List.nub $ map (mapeiaPosicao l a)
        [(x-1,y-1), (x, y-1), (x+1, y-1),
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
--    | x<l && y<a = let valor = c !! x !! y in traceStack ("celulas: " ++ show c ++ ", largura : " ++ show l ++ ", caltura: " ++ show a ++ ", Posicao: " ++ show (x,y)) $ c !! x !! y
    | x<l && y<a = c !! x !! y
    | otherwise  = ' '

totalVizinhosVivos :: Tabuleiro -> Posicao -> Int
totalVizinhosVivos tabuleiro (x,y) =
    contarElemento valorCelulaViva (map (valorCelula tabuleiro) (posicoesVizinhas tabuleiro (x,y)))

totalVizinhosMortos :: Tabuleiro -> Posicao -> Int
totalVizinhosMortos tabuleiro (x,y) =
    contarElemento valorCelulaMorta (map (valorCelula tabuleiro) (posicoesVizinhas tabuleiro (x,y)))

totalVizinhosZumbis :: Tabuleiro -> Posicao -> Int
totalVizinhosZumbis tabuleiro (x,y) =
    contarElemento valorCelulaZumbi (map (valorCelula tabuleiro) (posicoesVizinhas tabuleiro (x,y)))

condicaoReproducao :: Tabuleiro -> Posicao -> Bool
condicaoReproducao t0 pos =
    (valorCelulaMorta == valorCelula t0 pos) &&
    (totalVizinhosVivos t0 pos == 3)

condicaoInfeccao :: Tabuleiro -> Posicao -> Bool
condicaoInfeccao t0 pos =
    (valorCelulaViva == valorCelula t0 pos) &&
    (totalVizinhosZumbis t0 pos > 0)

condicaoSubpopulacao :: Tabuleiro -> Posicao -> Bool
condicaoSubpopulacao t0 pos =
    (valorCelulaViva == valorCelula t0 pos) &&
    (totalVizinhosVivos t0 pos < 2) &&
    (totalVizinhosZumbis t0 pos == 0)

condicaoSuperpopulacao :: Tabuleiro -> Posicao -> Bool
condicaoSuperpopulacao t0 pos =
    (valorCelulaViva == valorCelula t0 pos) &&
    (totalVizinhosVivos t0 pos > 3) &&
    (totalVizinhosZumbis t0 pos == 0)

condicaoInanicao :: Tabuleiro -> Posicao -> Bool
condicaoInanicao t0 pos =
    (valorCelulaZumbi == valorCelula t0 pos) &&
    (totalVizinhosVivos t0 pos == 0)

processaCelulasLinha :: Tabuleiro -> Posicao -> [Char]
processaCelulasLinha t0 (x,y)
    | x >= largura t0 = []
    | condicaoReproducao t0 (x,y) = valorCelulaViva:processaCelulasLinha t0 (x+1,y)
    | condicaoInfeccao t0 (x,y) = valorCelulaZumbi:processaCelulasLinha t0 (x+1,y)
    | condicaoSubpopulacao t0 (x,y) = valorCelulaMorta:processaCelulasLinha t0 (x+1,y)
    | condicaoSuperpopulacao t0 (x,y) = valorCelulaMorta:processaCelulasLinha t0 (x+1,y)
    | condicaoInanicao t0 (x,y) = valorCelulaMorta:processaCelulasLinha t0 (x+1,y)
    | otherwise = valorCelula t0 (x,y):processaCelulasLinha t0 (x+1,y)

processaLinhasTabuleiro :: Tabuleiro -> Posicao -> [[Char]]
processaLinhasTabuleiro t0 (x, y)
    | y >= altura t0 = []
    | otherwise = processaCelulasLinha t0 (x,y):processaLinhasTabuleiro t0 (x,y+1)

geraNovoTabuleiro :: Tabuleiro -> Tabuleiro
geraNovoTabuleiro t0 = Tabuleiro {
    celulas=processaLinhasTabuleiro t0 (0,0),
    largura=largura t0,
    altura=altura t0
}

iteraTabuleiro :: Tabuleiro -> Tabuleiro -> Int -> Int -> Tabuleiro
iteraTabuleiro tabuleiroAtual tabuleiroNovo i iMaximo
    | i >= iMaximo = tabuleiroNovo
    | celulas tabuleiroAtual == celulas tabuleiroNovo = tabuleiroNovo
    | otherwise = let resultado = iteraTabuleiro tabuleiroNovo (geraNovoTabuleiro tabuleiroNovo) (i+1) iMaximo in trace (show resultado ++ " i: " ++ show i) resultado
    -- | otherwise = iteraTabuleiro tabuleiroNovo (geraNovoTabuleiro tabuleiroNovo) (i+1) iMaximo

executaJogoVida :: Tabuleiro -> Int -> Tabuleiro
-- executaJogoVida tabuleiro iMaximo = iteraTabuleiro Tabuleiro{celulas=[[]],largura=0,altura=0} tabuleiro 0 iMaximo
executaJogoVida tabuleiro = iteraTabuleiro Tabuleiro{celulas=[[]],largura=0,altura=0} tabuleiro 0

-- main :: IO ()
-- main = someFunc
-- main = mapM_ process . takeWhile (/= "q") . lines =<< getContents
--   where process line = do -- whatever you like, e.g.
--                           putStrLn line

-- main = do 
--     print (contarElemento 'a' "abracadabra")

main = do
    -- let t0 = Tabuleiro {celulas=[[]],largura=0,altura=0}
    -- let t1 = Tabuleiro {celulas=["    ","  v ","    ","    "],largura=4,altura=4}
    -- let tabuleiroFinal = iteraTabuleiro t0 t1 0 2
    let t  = Tabuleiro {celulas=["    ","  v ","    ","    "],largura=4,altura=4}
    let tabuleiroFinal = executaJogoVida t 2
    print ("Tabuleiro apos as iteracoes: " ++ show tabuleiroFinal)

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