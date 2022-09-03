module Main where
import Data.Char
-- 1. Escreva  uma  função  chamada  fatorialn  que  usando  o  operador  range  e  a  função  foldr devolva o fatorial de n.
fatorialn :: Int -> Int
fatorialn n = foldr (*) 1 [1 .. n]

-- 2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos inteiros listados.
quadradoReal :: [Double] -> [Double]
quadradoReal = map (^ 2)

-- 3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras.
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras = map length

-- 4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29.
verificaMultiplo :: [Int] -> [Int]
verificaMultiplo = filter (\x -> x `mod` 29 == 0)

maiorMultiploDe29 :: [Int] -> Int
maiorMultiploDe29 listaNumeros = last (verificaMultiplo listaNumeros)

-- 5. Usando  a  função  filter  escreva  uma  função,  chamada  maiorMultiploDe que  recebe  um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.

verificaMultiploN :: Int -> [Int] -> [Int]
verificaMultiploN n = filter (\x -> x `mod` n == 0)

maiorMultiploDe :: Int -> Int
maiorMultiploDe n = last (verificaMultiploN n [0 .. 100000])

-- 6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=12 +22 +32 +42...+𝑛2.2 +42...+𝑛2..2 +42...+𝑛2...2 +42...+𝑛2. 
somaQuadrados :: Int -> Int
somaQuadrados n = foldr (+) 0 (map (^2) [1..n])


-- 7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.
comprimento :: [Int] -> Int
comprimento lista = foldl (+) 0 (map (^0) lista)


-- 8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.
--flip
--ord 
--max 
--min
--curry
aaa  = curry (\ (x,y) -> 2*x+y)

--uncurry
times = uncurry (*)
main = do

  print ("Func 1 - entrada: 5, resultado :  " ++ show (fatorialn 5))
  print ("Func 2 - entrada: [1,2,3,4], resultado :  " ++ show (quadradoReal [1, 2, 3, 4]))
  print ("Func 3 - entrada: [ovo, abacate, pneumoltramicroscopicosilicovulnanuconioptico], resultado :  " ++ show (comprimentoPalavras ["ovo", "abacate", "pneumoltramicroscopicosilicovulnanuconioptico"]))
  print ("Func 4 - entrada: [1.. 100000], resultado :  " ++ show (maiorMultiploDe29 [1 .. 100000]))
  print ("Func 5 - entrada: 47, resultado :  " ++ show (maiorMultiploDe 47))
  print ("Func 6 - entrada: 47, resultado :  " ++ show (somaQuadrados 9))
  print ("Func 7 - entrada [5,4,3,2,1], resultado :  " ++ show (comprimento [5,4,3,2,1]))
  print("Func 8 flip - entrada hello world, resultado :" ++ show (flip (++) "hello" "world"))
  print("Func 8 flip - entrada (^)2 3), resultado :" ++ show (flip(^)2 3))
  print("Func 8 ord - entrada a:, resultado :" ++ show (ord 'a' ))
  print("Func 8 ord - entrada @:, resultado :" ++ show (ord '@' ))
  print("Func 8 max - entrada 5 2:, resultado :" ++ show (max 5 2))
  print("Func 8 max - entrada 10 6:, resultado :" ++ show (max 10 6))
  print("Func 8 min - entrada 90 45:, resultado :" ++ show (min 90 45))
  print("Func 8 min - entrada 63 75:, resultado :" ++ show (min 63 75))
  print("Func 8 curry - entrada 2 3:, resultado :" ++ show (aaa 2 3))
  print("Func 8 curry - entrada 2 3:, resultado :" ++ show (curry fst 2 3))
  print("Func 8 uncurry - entrada 5 4:, resultado :" ++ show ( uncurry mod (5,4)))
  print("Func 8 uncurry - entrada 3 2:, resultado :" ++ show ( times (3,2)))

  