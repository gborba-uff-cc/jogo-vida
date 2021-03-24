module Main where

import Lib

main :: IO ()
main = someFunc

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