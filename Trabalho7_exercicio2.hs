--Beatriz de Assumpção zardo
--2. Usando Haskell, crie uma função chamada fatias, com a assinatura dada por fatias:: :: Int -> String -> [[Int]] que receba um string e um inteiro e devolva uma lista de listas contendo em cada item uma lista de inteiros. Esta função receberá strings contendo digitos como, por exemplo: "345234678" e devolverá listas parecidas com [[3,4,5],[4,5,2],[5,2,3],[2,3,4],[3,4,6],[4,6,7],[6,7,8]] No caso do exemplo, o inteiro que fatias recebeu foi 3. Observe que você poderá criar, quantas funções de apoio acredite que sejam necessárias para criar as funcionalidades de fatias inclusive, se achar interessante, podeusar as funções mapMaybe e digitToInt.

module Main where

import Data.Char (digitToInt)

charToInt :: Char -> Int
charToInt = digitToInt

fatias :: Int -> String -> [[Int]]
fatias i string = map (map charToInt) (fatias' i string)

fatias' :: Int -> String -> [String]
fatias' n string = if length string < n then [] else take n string : fatias' n (tail string)

main :: IO ()
main = do
  print $ fatias 3 "345123234678"
  