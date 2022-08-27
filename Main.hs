-- 1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando Haskell.
secFibonacci :: Int -> Int
secFibonacci 0 = 0
secFibonacci 1 = 1
secFibonacci a = secFibonacci (a -1) + secFibonacci (a -2)

-- 2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300  AC.  Podemos simplificar  este algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  entre  eles  será dado  pelo  valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma  função  para  o  cálculo  do  MDC  entre  dois  números  inteiros  positivos,  usando  o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.
mdc :: Int -> Int -> Int
mdc a b
  | b == 0 = a
  | otherwise = mdc b (mod a b)

-- 3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e recursividade.
somaNumeros :: Int -> Int
somaNumeros 0 = 0
somaNumeros a = mod a 10 + somaNumeros (a `quot` 10)

-- 4. Escreva  uma  função  que  devolva  a  soma  de  todos  os  números  menores  que  10000  que sejam múltiplos de 3 ou 5.
somaMenores :: Int -> Int -> Int
somaMenores 0 s = s
somaMenores a s
    | mod a 3 == 0 = somaMenores (a-1) (s + a) 
    | mod a 5 == 0 = somaMenores (a-1) (s + a) 
    | otherwise = somaMenores (a-1) s 

-- 5. Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  diferença  entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade. 
diferenca :: [Int] -> Int -> Int -> Int
diferenca [] somaQuadrados somaElevada = somaQuadrados - (somaElevada*somaElevada)
diferenca lista somaQuadrados somaElevada = diferenca (tail lista) (somaQuadrados + (head lista)*(head lista)) (somaElevada + head lista)

-- 6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado.
criaLista :: Int -> Int -> [Int] -> Int
criaLista nAlvo ncontador listaNumeros = criaLista nAlvo (ncontador-1) (ncontador : listaNumeros)

eliminaPrimos :: [Int] -> [Int] -> [Int]
eliminaPrimos listaNumeros listaPrimos = []

-- 7. Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  função  que  devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado.
calculoLucas :: Int -> Int
calculoLucas numero
          | numero == 0 = 2
          | numero == 1 = 1
          | otherwise = calculoLucas (numero -1) + calculoLucas(numero - 2)

montaListaLucas :: Int -> Int -> [Int] -> [Int]
montaListaLucas numeroParada numero listaLucas
      | numeroParada == 1 = []
      | numeroParada == 2 = []
      | calculoLucas numero > numeroParada = listaLucas
      | otherwise = montaListaLucas numeroParada (numero + 1) listaLucas ++ [(calculoLucas numero)]

-- 8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].
aoContrario :: [Int] -> [Int] -> [Int]
aoContrario [] listaContraria = listaContraria
aoContrario lista listaContraria = aoContrario (tail lista) ((head lista) : listaContraria)

-- 9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.

somaRecursiva :: Int -> Int -> Int -> Int
somaRecursiva a b soma
  | b == 1 = soma + a
  | otherwise = somaRecursiva a (b -1) (soma + a)

-- 10. Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.
comprimento :: [Int] -> Int -> Int
comprimento [] contador = contador
comprimento a contador = comprimento (tail a) (contador+1)

main :: IO ()
main = do

  print ("Func 1 - entrada: 6, resultado :  " ++ show (secFibonacci 6))
  print ("Func 2 - entrada: 3, resultado :  " ++ show (mdc 5 20))
  print ("Func 3 - entrada: 1234, resultado :  " ++ show (somaNumeros 1234))
  print ("Func 4 - entrada: 9999, resultado :  " ++ show (somaMenores 9999 0))
  print ("Func 5 - entrada: [1,5,9], resultado :  " ++ show (diferenca [1,5,9] 0 0))

  print ("Func 7 - entrada: 10 0 [], resultado :  " ++ show (montaListaLucas 10 0 []))
  print ("Func 8 - entrada: [1234], resultado :  " ++ show (aoContrario [1,2,3,4] []))
  print ("Func 9 - entrada: 4 5, resultado :  " ++ show (somaRecursiva 4 5 0))
  print ("Func 10 - entrada: [1,2,3,4,5,6], resultado :  " ++ show (comprimento [1,2,3,4,5,6] 0))