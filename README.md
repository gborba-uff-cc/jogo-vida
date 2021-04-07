# Jogo Vida

## Sobre o programa

Essa implementação do jogo da vida, pega um tabuleiro inicial e apresenta um tabuleiro final após iterar N vezes sobre o tabuleiro inicial ou até que não haja alteração entre dois tabuleiro consecutivos isto é, até que o tabuleiro atual seja igual ao tabuleiro anterior.

O arquivo de texto do tabuleiro não precisa necessáriamente ter uma formatação especifica, pois o programa lerá cada linha do arquivo de texto e filtrará todos os caracteres que são valores válidos para células, assim a linha "em um apocalipse zumbi voce sobreviveria por muito tempo" se tornam em uma linha de 9 células [m,m,z,m,v,v,v,m,m] no tabuleiro e, a linha "mmVmv mvMZzm vm" se torna em uma linha de 13 celulas [m,m,v,m,v,m,v,m,z,z,m,v,m]. Após a leitura das linhas o programa verifica se todas as linhas lidas tem o mesmo numero de células e caso tenham executa o algoritmo proposto.

Como acréscimo, esso jogo da vida considera que o tabuleiro dá a volta pelas bordas. Por exemplo no tabuleiro:

```
m m m m m
m v v v m
m m m m m
```

os vizinhos da célula do canto superior esquerdo são as células em maiúsculo a seguir:

```
m M _ _ M
M V _ _ M
M M _ _ M
```

e vizinhos que talvez apareçam mais de uma vez, em tabuleiros 2x2 por exemplo, cada vizinho será contabilizado apenas uma vez.

## Algoritmo proposto

Um novo tabuleiro será gerado a cada iteração, e em cada iteração o novo valor das células será decidido seguindo as seguintes regras:

* A célula passará de **morta** para **viva** caso existam 3 celulas vivas adjacentes (_reproducao_)
* A célula passará de **viva** para **zumbi** caso exista alguma células zumbi adjacentes (_infeccao_)
* A célula passará de **viva** para **morta** caso existam menos que 2 células vivas adjacentes e 0 zumbi adjacentes (_subpopulacao_)
* A célula passará de **viva** para **morta** caso existam mais que 3 células vivas adjacentes e 0 zumbi adjacentes (_superpopulacao_)
* A célula passará de **zumbi** para **morta** caso existam 0 células vivas adjacentes (_inanicao_)

## Compilacao

> Necessário ter o compilador ghc instalado.

### Linux

```bash
gch Main.hs
```

## Execucao

### Linux

```bash
./Main.hs
```

## Exemplo de uso

> Quando o programa pedir o nome do arquivo que contém o tabuleiro.

```
tabuleiro.txt
```

> Quando o programa pedir o número máximo de iterações sobre o tabuleiro.

```
2
```
