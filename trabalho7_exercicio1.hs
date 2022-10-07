--Beatriz de Assumpção zardo
-- 1. Utilizando a linguagem Haskell e o seu próprio tipo de dados, crie um programa capaz de classificar triângulos a partir do comprimento dos seus lados sabendo que o comprimento de cada um dos lados deve ser maior que zero e que:
-- a. triângulos equiláteros têm todos os lados do mesmo tamanho;

-- b. triangulos esosceles têm, no mínimo, dois lados do mesmo tamanho;

-- c. triangulos escalenos têm tem todos os lados de tamanho diferentes;

-- d. triangulos degenerados têm um lado igual a soma dos outros dois e área zero.
module Main where

data Triangulo = Equilatero | Isosceles | Escaleno | Degenerado
  deriving (Eq, Show)

classificador :: Float -> Float -> Float -> Triangulo
classificador la lb lc
  | la == lb && lb == lc = Equilatero
  | la == lb || lb == lc || la == lc = Isosceles
  | la /= lb && lb /= lc && la /= lc = Escaleno
  | otherwise = Degenerado

main :: IO ()
main = do
  print (classificador 1.0 1.0 1.0)
  print (classificador 1.0 1.0 2.0)
  print (classificador 1.0 2.0 3.0)
