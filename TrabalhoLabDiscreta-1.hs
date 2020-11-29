--Laboratório de Matemática Discreta - Trabalho 1
--Amanda Clara Ribeiro Gonçalves
--2020003550

--exercício 1
areaRetangulo :: Int -> Int -> Int
areaRetangulo w h = w*h

areaTriangulo :: Float -> Float -> Float
areaTriangulo b h = b * h / 2

--exercício 2
tempoGasto :: Float -> Float -> Float
tempoGasto s v = s/v

diferencaTempo :: Float -> Float -> Float -> Float
diferencaTempo s v1 v2 = s/v1 - s/v2

--exercício 3
funcao3 :: Float -> Float
funcao3 n
    | n == 0 = 1
    | n == 1 = 3
    | n == 2 = 3
    | n > 2 = funcao3 (n-1) + funcao3 (n-2)

--exercício 4
funcao4 :: Float -> Float -> Float
funcao4 m n
    | m == 0 = n + 1
    | (m > 0) && (n == 0) = funcao4 (m - 1) 1
    | (m > 0) && (n > 0) = funcao4 (m - 1) (funcao4 m (n - 1))

--exercício 5
funcao5 :: Float -> Float -> Float -> Float
funcao5 a1 a2 a3
    | a1 /= a2 && a1 /= a3 && a2 /= a3 = 1
    | a1 == a2 || a1 == a3 || a2 == a3 = 2
    | a1 == a2 && a1 == a3 = 3

--exercício 6
funcao6 :: Int -> Int -> Int
funcao6 a b
    | a < b = -1
    | a == b = 0
    | a > b = a*b

--exercício 7
funcao7 :: Int -> Int
funcao7 n
    | n == 0 = 0
    | mod n 2 == 1 = n + funcao7 (n-2)
    | mod n 2 /= 1 = funcao7 (n-2)

--exercício 8
funcao8:: Int -> Bool
funcao8 n
    | (n == 1) = False
    | otherwise = primo n 2
    where
        primo a b = if (a == b)
                       then True
                       else if (mod a b) == 0
                            then False
                            else primo a (b + 1)

--exercício 9
funcao9 :: Int -> Int -> Int -> Int
funcao9 a b c
    | a == 0 = b
    | a == 1 = c
    | a > 1 = funcao9 (a-1) c (b+c)

--exercício 10
funcao10 :: Float -> Float -> Float -> Float
funcao10 vi vf t = vi * t + (a*(t^2)) / 2
    where v = vf - vi
          a = v / t

funcao10b :: Float -> Float -> Float -> Float
funcao10b vi vf t = let a = vf - vi / t
                    in  vi * t + (a*(t^2)) / 2

--exercício 11
funcao11 :: Int -> Int -> Int
funcao11 a b
    | a == 0 = b
    | a < b = funcao11 (a-1) (b+1)
    | a > b = funcao11 (b-1) (a+1)

--exercício 12
funcao12 :: [Int] -> [Int] -> [Int]
funcao12 [] [] = []
funcao12 (h1:t1) (h2:t2)
    | (h1:t1) /= (h2:t2) =  [h1] ++ [h2] ++ (funcao12 t1 t2)

--exercício 13
funcao13 :: Int -> [Int] -> Int
funcao13 n [] = -1
funcao13 0 (h:_) = h
funcao13 n (h:t) = funcao13 (n-1) t

--exercício 14
funcao14 :: Int -> [Int] -> Int -> [Int]
funcao14 n [] x = []
funcao14 0 (h:t) x = (x:t)
funcao14 n (h:t) x = [h] ++ funcao14 (n-1) t x

--exercício 15
funcao15 :: [Char] -> Char -> Char -> [Char]
funcao15 [] _ _ = []
funcao15 (h:t) a b
    | h == a = [b] ++ funcao15 t a b
    | otherwise = [h] ++ funcao15 t a b

--exercício 16
funcao16 :: [Char] -> Bool
funcao16 l = igual (inverte l) l
  where
    igual [] [] = True
    igual (h1:t1) (h2:t2)
      | h1 /= h2 = False
      | otherwise = igual t1 t2
    inverte [] = []
    inverte (h:t) = (inverte t) ++ [h]

--exercício 17
iniciais :: [Int] -> Int -> [Int]
iniciais [] _ = []
iniciais (h:_) 1 = [h]
iniciais (h:t) n = [h] ++ iniciais t (n-1)

finais :: [Int] -> Int -> [Int]
finais [] _ = []
finais (h:t) n = if (tamanho (h:t) == (n+1))
                 then t
                 else finais t n
  where
    tamanho [] = 0
    tamanho (h:t) = 1 + tamanho t

--exercício 18
prefixo :: [Int] -> [Int] -> Bool
prefixo [] _ = True
prefixo (h1:t1) (h2:t2) = if (h1 == h2)
                          then prefixo t1 t2
                          else False

--exercício 19
tam :: [Int] -> Int
tam [] = 0
tam (h:t) = 1 + tam t

sozinhos :: [[Int]] -> [[Int]]
sozinhos [] = []
sozinhos (h:t) = if (tam h == 1)
                 then h:(sozinhos t)
                 else sozinhos t

--exercício 20
positivos :: [Int] -> [Int]
positivos [] = []
positivos (h:t)
    | h >= 0 = [h] ++ positivos t
    | h < 0 = positivos t
