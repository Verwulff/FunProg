{-
Реализуйте функции foldl и foldr из лекции

На основе функций foldl и foldr реализуйте свои версии функций

map :: (a -> b) -> [a] -> [b]  
flatMap :: (a -> [b]) -> [a] -> [b]  
concat :: [a] -> [a] -> [a]  
filter :: (a -> Boolean) -> [a] -> [a]  
maxBy :: (a -> Integer) -> [a] -> a  
minBy :: (a -> Integer) -> [a] -> a  
reverse :: [a] -> [a]  
elementAt :: Integer -> [a] -> a  
indexOf :: String -> [String] -> Integer   
-}


module Main where

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ parameter [] = parameter
foldl function parameter (element : other) = foldl function (function parameter element) other

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ parameter [] = parameter
foldr function parameter (element : other) = function element (foldr function parameter other)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map function list = foldr (\ element concatable -> function element : concatable) [] list

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap _ [] = []
flatMap function list = foldl (\ concatable element -> concatable ++ function element) [] list

concat :: [a] -> [a] -> [a]
concat left [] = left
concat [] right = right
concat left right = foldr (:) right left

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter function list = foldr (\ element concatable -> if function element then element : concatable else concatable) [] list

maxBy :: (a -> Integer) -> [a] -> a
maxBy _ [] = error "Empty list"
maxBy function list = foldl (\ greatest element -> if function element > function greatest then element else greatest) (head list) list

minBy :: (a -> Integer) -> [a] -> a
minBy _ [] = error "Empty list"
minBy function list = foldl (\ least element -> if function element < function least then element else least) (head list) list

reverse :: [a] -> [a]
reverse [] = []
reverse list = foldl (\ concatable element -> element : concatable) [] list

elementAt :: Integer -> [a] -> a
elementAt _ [] = error "Index out of bounds"
elementAt at list
                  | at < 0 = error "Index out of bounds"
                  | at == 0 = head list
                  | otherwise = foldr (\ element hack parameter -> case parameter of
                                          0 -> element
                                          _ -> hack (parameter - 1)) (\ _ -> error "Index out of bounds!") list at

indexOf :: Eq a => a -> [a] -> Integer
indexOf _ [] = -1
indexOf value list
                  | result >= 0 = result
                  | otherwise = -1
                  where
                    result = foldl (\ index element -> if element == value && index < 0 then abs index - 1 else if index < 0 then index - 1 else index) (-1) list

main :: IO ()
main = putStrLn $ show (foldl max (-100) [1, -3, 6, -4, -9]) ++ "\n" ++
                  show (foldr max (-100) [1, -3, 6, -4, -9]) ++ "\n" ++
                  show (map abs [1, -3, 6, -4, -9]) ++ "\n" ++
                  show (flatMap (\ number -> [number, number]) [1, -3, 6, -4, -9]) ++ "\n" ++
                  show (concat [23, 54, 66] [1, -3, 6, -4, -9]) ++ "\n" ++
                  show (filter (\ number -> number /= 6) [1, -3, 6, -4, -9, 6, 6]) ++ "\n" ++
                  show (maxBy abs [1, -3, 6, -4, -9, 6, 6]) ++ "\n" ++
                  show (minBy abs [1, -3, 6, -4, -9, 6, 6]) ++ "\n" ++
                  show (reverse [1, -3, 6, -4, -9, 6, 6]) ++ "\n" ++
                  show (elementAt 15000000 [1..15000001]) ++ "\n" ++
                  show (elementAt 4 [1, -3, 6, -4, -9, 6, 6]) ++ "\n" ++
                  show (indexOf "string" ["int", "double", "string", "float", "long", "string"]) ++ "\n" ++
                  show (indexOf 1 [2, 3, 4, 1, 9, 6]) ++ "\n" ++ ""
