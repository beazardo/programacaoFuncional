-- #Aluna: Beatriz de Assumpção Zardo#-}

-- # 1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.  #-}

soma1 :: Int -> Int 
soma1 x = x + 1

-- # 2. Escreva  uma  função  chamada  sempre  que,  não  importando  o  valor  de  entrada,  devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo.

sempre :: String -> Int
sempre a = 0

--  3. Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto  flutuantes  com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. # -}

treco :: Float -> Float -> Float -> Float
treco v1 v2 v3  = (v1 + v2) * v3

-- #4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiro.#-}

resto :: Int -> Int -> Int
resto n1 n2 = mod n1 n2

-- #5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários. #-}

precoMaior :: (Ord valores) => [valores] -> valores 
precoMaior [] = error "Lista vazia"
precoMaior [valores] = valores
precoMaior (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = precoMaior xs

-- # 6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar #-}

impar :: [Int] -> Bool
impar [a,b] = if even(a*b) then False else True

-- # 7. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros. #-}
somaPar :: (Int, Int) -> Int
somaPar (a, b) = a + b

-- # 8. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥2 +𝑦2 +𝑧. #-}
equacao :: Double -> Double -> Double -> Double
equacao x y z = x*x + y*y +z

-- # 9. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos (cuidadospelavida.com.br).  Observe  que  este  diagnóstico  é  meramente  estatístico  e  não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico.  #-}

diagnostico :: Double -> Double -> IO()
diagnostico peso altura 
    | 17 > resultado  = putStrLn "Muito abaixo do peso"
    | 18.49 > resultado && 17 < resultado = putStrLn "Abaixo do peso"
    | 24.99 > resultado && 18.5 < resultado = putStrLn "Peso normal"
    | 29.99 > resultado && 25 < resultado = putStrLn "Sobrepeso"
    | 30 > resultado && 34.99 < resultado = putStrLn "Obesidade Leve"
    | 35 > resultado && 39.99 < resultado = putStrLn "Obesidade Severa"
    | 40 < resultado = putStrLn "Obesidade Morbida"
    | otherwise = putStrLn "Dados incorretos, nao foi possivel calcular"
  where resultado = peso/(altura^2)

-- # 10. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 #-}
bissexto :: Int -> Bool
bissexto ano 
  | (mod ano 4) == 0 && (mod ano 100) == 0 && (mod ano 400) == 0 = True
  | (mod ano 4) == 0 && (mod ano 100) /= 0 = True
  | otherwise = False
   

                        
main :: IO()
main = do
  print("Func 1 - entrada: 3, resultado :  " ++ show (soma1 3))
  print("Func 1 - entrada: -6, resultado :  " ++ show (soma1 (-6)))
  print("Func 2 - entrada: coisa, resultado : " ++ show (sempre "coisa"))
  print("Func 3 - entrada: 4.00 5.00 2.00, resultado: " ++ show (treco 4.00 5.00 2.00)) 
  print("Func 4 - entrada: 10 5, resultado: " ++ show (resto 10 5))
  print("Func 4 - entrada: 5 2, resultado: " ++ show (resto 5 2))
  print("Func 5 - entrada: 1,2,3,4, resultado: " ++ show (precoMaior [1,2,3,4]))
  print("Func 6 - entrada: 3 5, resultado: " ++ show (impar [3,5]))
  print("Func 6 - entrada: 2 8, resultado: " ++ show (impar [2,8]))
  print("Func 7 - entrada: 3 4, resultado: " ++ show (somaPar (3, 4)))
  print("Func 8 - entrada: 2.0 5.0 8.0, resultado: " ++ show (equacao 2.0 5.0 8.0))
  print("Func 9 - entrada: 77.0 1.80, resultado: ") 
  diagnostico 77.0 1.80
  print("Func 9 - entrada: 61.0 1.55, resultado: ") 
  diagnostico 61.0 1.55
  print("Func 10 - entrada 1996, resultado: " ++ show (bissexto 1996))
  print("Func 10 - entrada 2022, resultado: " ++ show (bissexto 2022))