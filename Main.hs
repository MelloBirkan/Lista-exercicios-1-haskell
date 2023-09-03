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

-- Main
main :: IO ()
main = do
    let lista = [1, 1, 1, 2, 2, 2, 3, 3, 3, 4]
    print $ tot3 lista
    print $ rev lista
    print $ ult lista
    print $ penult lista
    print $ seg lista
