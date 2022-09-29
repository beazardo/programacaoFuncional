-- Beatriz de Assumpção Zardo
module Main where

-- 1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado.

divisoresden :: Int ->  [Int]
divisoresden nAlvo = [a | a <- [1..nAlvo], mod nAlvo a ==0]

--2. Usando  List Comprehension  escreva  uma  função,  chamada  contaCaractere,  que  conte  a ocorrência de um caractere específico, em uma string dada.

contaCaractere :: Char -> [Char] -> Int
contaCaractere caracAlvo palavra = sum [1 | carac <- palavra, caracAlvo == carac]

--3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada.

dobroNaoNegativo :: [Int] -> [Int] -> [Int]
dobroNaoNegativo [] listaTemp = listaTemp
dobroNaoNegativo listaA listaTemp
  | last listaA < 0 = dobroNaoNegativo (init listaA) (listaTemp)
  | otherwise = dobroNaoNegativo (init listaA) ((last listaA * 2) : listaTemp)

--4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado.

pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], b - c < a, a < b + c, a - c < b, b < a + c, a - b < c, c < a + b]

--5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado.

numerosPerfeitos :: Int -> [Int]
numerosPerfeitos nAlvo = [a | a <- [1 .. nAlvo], sum (init (divisoresden a)) == a]

--6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar [] _ = 0
produtoEscalar listaA listaB = sum [a * b | (a, b) <- zip listaA listaB]

--7. Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos,  que  devolva uma lista contendo os n primeiros números primos a partir do número 2.

primeirosPrimos :: Int -> [Int]
primeirosPrimos nAlvo = [a | a <- [2..nAlvo], (divisoresden a) == [1]]


--8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes.

paresOrdenados :: Int -> [(Int, Int)]
paresOrdenados nAlvo = [(2 ^ a, 3 ^ b) | a <- [1 .. nAlvo], b <- [1 .. nAlvo]]

main :: IO ()
main = do
  print ("Func 1: entrada 9 resultado: " ++ show (divisoresden 9 ))

  print ("Func 2: entrada b bomba resultado: " ++ show (contaCaractere 'b' ['b', 'o', 'm', 'b', 'a']))

  print ("Func 3: entrada 1,2,3,4,-5 [] resultado: " ++ show (dobroNaoNegativo [1, 2, 3, 4, -5] []))

  print ("Func 4: entrada 5  resultado: " ++ show (pitagoras 5))

  print ("Func 5: entrada 5  resultado: " ++ show (numerosPerfeitos 100))

  print ("Func 6: entrada [1,2,3,4] [5,6,7,8] resultado: " ++ show (produtoEscalar [1, 2, 3, 4] [5, 6, 7, 8]))

  print ("Func 7: entrada 10 resultado: " ++ show (primeirosPrimos 10))

  print ("Func 8: entrada 5 resultado: " ++ show (paresOrdenados 5))
