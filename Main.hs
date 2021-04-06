module Main where

import qualified Data.List (delete, length, nub, partition)
import qualified Data.Char (toUpper)

type Posicao = (Int ,Int)
data Tabuleiro = Tabuleiro {
    celulasTabuleiro :: [[Char]],
    larguraTabuleiro :: Int,
    alturaTabuleiro  :: Int}
    deriving (Show)

valorCelulaViva :: Char
valorCelulaViva = 'V'

valorCelulaMorta :: Char
valorCelulaMorta = 'M'

valorCelulaZumbi :: Char
valorCelulaZumbi = 'Z'

valoresValidosCelula :: [Char]
valoresValidosCelula = [valorCelulaViva,valorCelulaMorta,valorCelulaZumbi]

posicaoValida :: Tabuleiro -> Posicao -> Bool
posicaoValida t (x,y) =
    0 <= x && x <= larguraTabuleiro t &&
    0 <= y && y <= alturaTabuleiro t

restringePosicao :: Int -> Int -> Posicao -> Posicao
restringePosicao valMaxX valMaxY (x, y) = (x `mod` valMaxX, y `mod` valMaxY)

posicoesVizinhas :: Tabuleiro -> Posicao -> [Posicao]
posicoesVizinhas Tabuleiro {larguraTabuleiro=l,alturaTabuleiro=a} (x, y) =
    -- delete((x,y), listaSemRepeticao(listaVizinhos))
    Data.List.delete (x,y) $ Data.List.nub $ map (restringePosicao l a)
        [(x-1,y-1), (x, y-1), (x+1, y-1),
         (x-1,y) ,{-(x, y),-} (x+1,y)   ,
         (x-1,y+1), (x, y+1), (x+1, y+1)]

contarElemento :: Eq a => a -> [a] -> Int
contarElemento e [] = 0
contarElemento e (x:xs)
    | e == x    = 1 + contarElemento e xs
    | otherwise = contarElemento e xs

-- precisa chamar retringePosicao antes
valorCelula :: Tabuleiro -> Posicao -> Char
valorCelula Tabuleiro {
    celulasTabuleiro=c,
    larguraTabuleiro=l,
    alturaTabuleiro=a}
    (x, y) = c !! y !! x
--    | x<l && y<a = let valor = c !! x !! y in traceStack ("celulas: " ++ show c ++ ", larguraTabuleiro : " ++ show l ++ ", calturaTabuleiro: " ++ show a ++ ", Posicao: " ++ show (x,y)) $ c !! x !! y
--    | otherwise  = ' '

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
    | x >= larguraTabuleiro t0 = []
    | condicaoReproducao t0 (x,y) = valorCelulaViva:processaCelulasLinha t0 (x+1,y)
    | condicaoInfeccao t0 (x,y) = valorCelulaZumbi:processaCelulasLinha t0 (x+1,y)
    | condicaoSubpopulacao t0 (x,y) = valorCelulaMorta:processaCelulasLinha t0 (x+1,y)
    | condicaoSuperpopulacao t0 (x,y) = valorCelulaMorta:processaCelulasLinha t0 (x+1,y)
    | condicaoInanicao t0 (x,y) = valorCelulaMorta:processaCelulasLinha t0 (x+1,y)
    | otherwise = valorCelula t0 (x,y):processaCelulasLinha t0 (x+1,y)

processaLinhasTabuleiro :: Tabuleiro -> Posicao -> [[Char]]
processaLinhasTabuleiro t0 (x, y)
    | y >= alturaTabuleiro t0 = []
    | otherwise = processaCelulasLinha t0 (x,y):processaLinhasTabuleiro t0 (x,y+1)

geraNovoTabuleiro :: Tabuleiro -> Tabuleiro
geraNovoTabuleiro t0 = Tabuleiro {
    celulasTabuleiro=processaLinhasTabuleiro t0 (0,0),
    larguraTabuleiro=larguraTabuleiro t0,
    alturaTabuleiro=alturaTabuleiro t0
}

iteraTabuleiro :: Tabuleiro -> Tabuleiro -> Int -> Int -> Tabuleiro
iteraTabuleiro tabuleiroAtual tabuleiroNovo i iMaximo
    | i >= iMaximo = tabuleiroNovo
    | celulasTabuleiro tabuleiroAtual == celulasTabuleiro tabuleiroNovo = tabuleiroNovo
    | otherwise = iteraTabuleiro tabuleiroNovo (geraNovoTabuleiro tabuleiroNovo) (i+1) iMaximo

executaJogoVida :: Tabuleiro -> Int -> Tabuleiro
-- executaJogoVida tabuleiro iMaximo = iteraTabuleiro Tabuleiro{celulasTabuleiro=[[]],larguraTabuleiro=0,alturaTabuleiro=0} tabuleiro 0 iMaximo
executaJogoVida t =
    iteraTabuleiro
        Tabuleiro {
            celulasTabuleiro=[[]],
            larguraTabuleiro=0,
            alturaTabuleiro=0}
        t
        0

-- FUNCOES PARA INTERACAO COM USUARIO

linhaMatrizParaTexto :: [Char] -> [Char]
linhaMatrizParaTexto [] = ""
linhaMatrizParaTexto [c1, c2] = [c1, ' ', c2]
linhaMatrizParaTexto (c1:c2:cs) = c1:' ':linhaMatrizParaTexto (c2:cs)

matrizParaTexto :: [[Char]] -> [Char]
matrizParaTexto [] = ""
matrizParaTexto [[]] = ""
matrizParaTexto [l1, l2] = linhaMatrizParaTexto l1 ++ "\n" ++ linhaMatrizParaTexto l2
matrizParaTexto (l1:ls) = linhaMatrizParaTexto l1 ++ "\n" ++ matrizParaTexto ls

tabuleiroParaString :: Tabuleiro -> [Char]
tabuleiroParaString t = matrizParaTexto $ celulasTabuleiro t

removeCharsIndesejados :: [Char] -> [Char]
-- removeCharsIndesejados l = filter (`elem` valoresValidosCelula) l
removeCharsIndesejados = filter (`elem` valoresValidosCelula)

textoParaMatriz :: [Char] -> [[Char]]
textoParaMatriz [] = [[]]
textoParaMatriz texto = map removeCharsIndesejados (lines $ map Data.Char.toUpper texto)

matrizDoArquivo :: FilePath -> IO [String]
matrizDoArquivo nArquivo = do
    conteudo <- readFile nArquivo
    let m = textoParaMatriz conteudo
    return m

matrizValida :: [[Char]] -> Bool
matrizValida [] = False
matrizValida m0 = all (\ l0 -> celulasValidas l0 && tamanhoValido l0) m0
    where celulasValidas l0 = null $ snd $ Data.List.partition (`elem` valoresValidosCelula) l0
          tamanhoValido [] = False
          tamanhoValido l1 = (==tamanhoHead) $ tamanhoLinha l1
          tamanhoLinha l2 = Data.List.length l2
          tamanhoHead = tamanhoLinha $ head m0

criaTabuleiro :: [[Char]] -> Tabuleiro
criaTabuleiro m = Tabuleiro {
    celulasTabuleiro=m,
    larguraTabuleiro=Data.List.length m,
    alturaTabuleiro=Data.List.length $ head m}

adquireInput :: IO (String, String)
adquireInput = do
    putStrLn "Jogo da Vida"
    putStrLn (
        "OBS.1: Cada linha do arquivo de entrada deve representar uma linha do tabuleiro.\n" ++
        "OBS.2: No arquivo de entrada:\n" ++
        "    " ++ valorCelulaViva:" representa uma celula viva\n" ++
        "    " ++ valorCelulaMorta:" representa uma celula morta\n" ++
        "    " ++ valorCelulaZumbi:" representa uma celula zumbi\n" ++
        "Digite o nome de um arquivo que contém o tabuleiro inicial.")
    nArquivo <- getLine
    putStrLn "Entre com o numero máximo de iterações que serão realizadas:"
    maxIteracoes <- getLine
    return (nArquivo, maxIteracoes)

main = do
    (nArquivo, maxIteracoes) <- adquireInput
    m <- matrizDoArquivo nArquivo
    let tabuleiro = criaTabuleiro m
    let tabuleiroFinal = executaJogoVida tabuleiro $ read maxIteracoes
    putStrLn ("Tabuleiro após as iterações:\n" ++ tabuleiroParaString tabuleiroFinal)

{- SECTION - Regras
NOTE - reproducao     - morta -> viva : =3 celulas vivas adjacentes
NOTE - infeccao       - viva -> zumbi : >0 celulas zumbi adj.
NOTE - subpopulacao   - viva -> morta : <2 celulas vivas adj. e 0 zumbi adj.
NOTE - superpopulacao - viva -> morta : >3 celulas vivas adj. e 0 zumbi adj.
NOTE - inanicao       - zumbi -> morta : =0 celulas vivas adj.
TODO - Regras
!SECTION -}