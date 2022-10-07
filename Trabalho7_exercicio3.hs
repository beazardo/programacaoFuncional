--Beatriz de Assumpção zardo
--3. Usando  Haskell  escreva  uma  função  chamada  romanos  que  receba  um  inteiro  menor  ou igual a 3000 e devolva um string deste inteiro representado com algarismos romanos. 
 module Main where

dicRomano = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"),
           (90,"XC"), (50,"L"), (40,"XL"), (10,"X"), (9,"IX"), (5,"V"),
           (4,"IV"), (1,"I")]


romanos :: Integer -> String
romanos 0 = "N"
romanos x = romanos' x

romanos' :: Integer -> String
romanos' x 
  | x == 0 = ""
  | x > 0 = b ++ romanos' (x - a)
      where (a, b) = head $ filter ((<= x) . fst) dicRomano

main :: IO()
main = do
  print(romanos 14)
