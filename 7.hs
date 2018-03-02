module Main where

-- Data.Sequence is not fair :(

data Deque a = Deque [a] [a] Int deriving (Show, Eq)

-- Best: O(1), Worst: O(1)
_emptyDequeue :: Deque t
_emptyDequeue = Deque [] [] 0

-- Best: O(1), Worst: O(1)
_sizeDequeue :: Deque t -> Int
_sizeDequeue (Deque _ _ size) = size

-- Best: O(1), Worst: O(1)
_pushBack :: Deque t -> t -> Deque t
_pushBack (Deque front back size) value = Deque front (value : back) (size + 1)

-- Best: O(1), Worst: O(1)
_pushFront :: Deque t -> t -> Deque t
_pushFront (Deque front back size) value = Deque (value : front) back (size + 1)

-- Best: O(1), Worst: ~O(N)
_popBack :: Deque t -> (Deque t, t)
_popBack (Deque [] [] _) = error "Deque is empty!"
_popBack (Deque front (element : other) size) = (Deque front other (size - 1), element)
_popBack (Deque front [] size) = _popBack $ Deque (fst splitResult) (reverse $ snd splitResult) size
                                 where
                                     frontSize = quot size 2
                                     splitResult = splitAt frontSize front

-- Best: O(1), Worst: ~O(N)
_popFront :: Deque t -> (Deque t, t)
_popFront (Deque [] [] _) = error "Deque is empty!"
_popFront (Deque (element : other) back size) = (Deque other back (size - 1), element)
_popFront (Deque [] back size) = _popFront $ Deque (reverse $ snd splitResult) (fst splitResult) size
                              where
                                  backSize = quot size 2
                                  splitResult = splitAt backSize back

{-
    Метод банкира:
    Для получения очереди размера N необходимо получить N$.
    Так как перемещается только половина списка, перемещение списка происходит не чаще одного раза в N/2, то есть линенйное число операций.
    Таким образом амортизированная слоность - константа.
    Метод физика:
    Потенциал - размер очереди.
    Добавление в непустую очередь увеличивает потенциал на 1.
    Удаление без перемещения не меняет потенциал.
    Удаление с перемещением уменьшает потенциал на N.
    Потенциал не отрицателен, потому что перемещаемое количество элементов не может быть больше размера очереди, а между перемещениями происходит не менее N/2 операций.
-}

main :: IO ()
main = putStrLn $ show (_popBack $ _pushBack (_pushBack (_pushBack _emptyDequeue (0 :: Integer)) 1) 2) ++ "\n" ++
show (_popFront $ _pushBack (_pushBack (_pushBack _emptyDequeue (0 :: Integer)) 1) 2)
