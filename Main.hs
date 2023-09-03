-- Exercício 1
tot3 :: [Int] -> [Int]
tot3 [] = []
tot3 xs
    | length prefix < 3 = [sum prefix]
    | otherwise = sum prefix : tot3 suffix
    where (prefix, suffix) = splitAt 3 xs

-- Exercício 2
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- Exercício 3
ult :: [a] -> [a]
ult [] = []
ult xs = (drop(length xs -1) xs)

-- Exercício 4
penult :: [a] -> a
penult [_] = error "A lista precisa ter pelo menos dois elementos."
penult [] = error "A lista precisa ter pelo menos dois elementos."
penult [x,_] = x
penult (_:xs) = penult xs

-- Exercício 5
seg :: [a] -> a
seg [_] = error "A lista precisa ter pelo menos dois elementos."
seg [] = error "A lista precisa ter pelo menos dois elementos."
seg [_, x] = x 
seg (x:y:xs) = y

-- Exercício 6
del_rep :: Eq a => [a] -> [a]
del_rep [] = []
del_rep (x:xs) = x : del_rep [y | y <- xs, y/= x] 

-- Exercício 7
totk :: Int -> [Int] -> [Int]
totk _ [] = [] 
totk k xs 
    | otherwise = total : totk k restante
     where
        total = sum (take k xs)
        restante = drop k xs

-- Exercício 8
trok2 :: [a] -> [a]
trok2 [] = []
trok2 (x:y:restante) = y : x : trok2 restante


-- Exercício 9
delk :: Int -> [a] -> [a]
delk _ [] = []  
delk k xs = deleta k xs k
  where 
  deleta :: Int -> [a] -> Int -> [a]
  deleta _ [] _ = [] 
  deleta y (x:resto) 1 = deleta y resto y
  deleta y (x:resto) n = x : deleta y resto (n - 1) 



-- Main
main :: IO ()
main = do
    let lista = [1, 1, 1, 2, 2, 2, 3, 3, 3, 4]
    let k = 5
    let lista2 = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

    print $ tot3 lista
    print $ rev lista
    print $ ult lista
    print $ penult lista
    print $ seg lista
    print $ del_rep lista
    print $ totk k lista2
    print $ trok2 lista
    print $ delk k lista 
