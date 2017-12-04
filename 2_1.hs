{-
Реализуйте структуру данных "бинарное дерево поиска" для целых чисел без балансировки. Реализация включает функции:
  Добавления элемента: insert :: BinaryTree -> Integer -> BinaryTree
  Удаления элемента: remove :: BinaryTree -> Integer -> BinaryTree
  Создания пустого дерева: emptyTree :: BinaryTree
  Поиска элемента в дереве: containsElement :: BinaryTree -> Integer -> Bool
  Поиска в дереве наименьшего элемента, который больше или равен заданному: nearestGE :: BinaryTree -> Integer -> Integer
  Создания дерева из списка: treeFromList :: [Integer] -> BinaryTree
  Создания списка из дерева: listFromTree :: BinaryTree -> [Integer]
Операторы insert и remove должны поддерживать цепочки вызовов в инфиксной форме:
  listFromTree (emptyTree `insert` 1 `insert` 2 `insert` 3) === [1,2,3]
-}

ata BinaryTree = EmptyTree
                | Leaf Integer
                | Node Integer BinaryTree BinaryTree deriving Show

insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree x = Leaf x
insert (Leaf k) x = insert (Node k EmptyTree EmptyTree) x
insert (Node k l r) x | x < k = Node k (insert l x) r
      | x > k = Node k l (insert r x)
      | k == x = Node k l r

remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree _ = EmptyTree
remove (Leaf l) x = if (l == x) then EmptyTree else Leaf l
remove (Node v l r) x | x < v = Node v (remove l x) r
                      | x > v = Node v l (remove r x)
                      | otherwise = concat l r
                  where concat EmptyTree t = t
                        concat (Leaf l) t = Node l EmptyTree t
                        concat (Node v l r) t = Node v l (concat r t)

emptyTree :: BinaryTree
emptyTree = EmptyTree

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Leaf l)  x | (l == x) = True
                            | otherwise = False
containsElement (Node v l r) x | x < v = containsElement l x
                               | x > v = containsElement r x
                               | otherwise = v == x                          

nearestGE:: BinaryTree -> Integer -> Integer
nearestGE EmptyTree _ = error "Not found"
nearestGE (Leaf val) target = if val >= target then val else error "Not found"
nearestGE (Node val left right) target | val < target = nearestGE right target
                                       | (isEmpty left) || (rightMost left < target) = val
                                       | otherwise = case left of 
                                        (Leaf v) -> v
                                        otherwise -> nearestGE left target

rightMost:: BinaryTree -> Integer
rightMost (Leaf v) = v
rightMost (Node v l r) = 
    case r of 
        (EmptyTree) -> v
        (Leaf v') -> v'
        (Node v' l' r') -> rightMost r

isEmpty:: BinaryTree -> Bool
isEmpty bt = 
    case bt of 
        (EmptyTree) -> True
        otherwise -> False                                             


treeFromList :: [Integer] -> BinaryTree
treeFromList = foldl insert EmptyTree

listFromTree :: BinaryTree -> [Integer]
listFromTree EmptyTree = []
listFromTree (Leaf l) = [l]
listFromTree (Node v l r) = (listFromTree l) ++ [v] ++ (listFromTree r)
