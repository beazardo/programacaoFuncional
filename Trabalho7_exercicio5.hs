--Beatriz de Assumpção zardo
-- 5. Usando a linguagem Haskell escreva uma função, chamada ultimoNome que receba o nome
-- completo de uma pessoa e devolva apenas o último sobrenome sem qualquer vogal. Caso
-- o  ultimo  sobrenome  não  contenhuma  nenhuma  vogal  devolva  o  ultimo  sobrenome  que
-- ainda contenha vogal. Por exemplo se o nome for Ana Maria stzrx, a função deve devolver
-- Maria, se o nome for Silvia Silva a função deve devolver Slv.

module Main where

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

hasVowel :: String -> Bool
hasVowel = any isVowel

removeVowels :: String -> String
removeVowels = filter (not . isVowel)

catchLastNameWithVowels :: String -> String
catchLastNameWithVowels name = if hasVowel lastName then lastName else catchLastNameWithVowels (init name)
  where lastName = last $ words name

ultimoNome :: String -> String
ultimoNome name = if hasVowel lastName then removeVowels lastName else catchLastNameWithVowels name
  where
    lastName = last $ words name

main :: IO ()
main = do
  print $ ultimoNome "Ana Maria stzrx"
  print $ ultimoNome "Silvia Silva"
  print $ ultimoNome "Beatriz zardo"