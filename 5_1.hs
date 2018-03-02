module Main where

import Control.Lens

data UnaryOperator = UnaryPlus | UnaryMinus deriving (Show, Eq)

data BinaryOperator = BinaryPlus | BinaryMinus | BinaryMult deriving (Show, Eq)

data Term = IntConstant { _constant :: Int }
            | Variable { _variable :: String }
            | UnaryTerm { _central :: Term, _uoperator :: UnaryOperator }
            | BinaryTerm { _left :: Term, _right :: Term, _boperator :: BinaryOperator } deriving (Show, Eq)

data ReverseList a = RNil
                     | RCons (ReverseList a) a deriving (Show, Eq)

variableLens :: Lens' Term String
variableLens = lens _variable (\term variable -> term { _variable = variable })

unaryOperatorLens :: Lens' Term UnaryOperator
unaryOperatorLens = lens _uoperator (\term operator -> term { _uoperator = operator })

binaryOperatorLens :: Lens' Term BinaryOperator
binaryOperatorLens = lens _boperator (\term operator -> term { _boperator = operator })

leftLens :: Lens' Term Term
leftLens = lens _left (\term left -> term { _left = left })

centralLens :: Lens' Term Term
centralLens = lens _central (\term central -> term { _central = central })

rightLens :: Lens' Term Term
rightLens = lens _right (\term right -> term { _right = right })

tailLens :: Lens' (ReverseList t) (ReverseList t)
tailLens = lens getter setter
          where
              getter RNil = error "List is empty!"
              getter (RCons list _) = list
              setter RNil _ = error "List is empty!"
              setter (RCons _ helement) value = RCons value helement

headLens :: Lens' (ReverseList t) t
headLens = lens getter setter
           where
               getter RNil = error "List is empty!"
               getter (RCons RNil helement) = helement
               getter (RCons list _) = getter list
               setter RNil _ = error "List is empty!"
               setter (RCons RNil _) value = RCons RNil value
               setter (RCons list _) value = setter list value

main :: IO ()
main = putStrLn $ show (set variableLens "SomeNewText" (Variable "SomeOldText")) ++ "\n" ++
                  show (view variableLens (Variable "SomeOldText")) ++ "\n" ++
                  show (set unaryOperatorLens UnaryPlus (UnaryTerm (IntConstant 1) UnaryMinus)) ++ "\n" ++
                  show (view unaryOperatorLens (UnaryTerm (IntConstant 1) UnaryMinus)) ++ "\n" ++
                  show (set binaryOperatorLens BinaryPlus (BinaryTerm (IntConstant 1) (IntConstant 1) BinaryMinus)) ++ "\n" ++
                  show (view binaryOperatorLens (BinaryTerm (IntConstant 1) (IntConstant 1) BinaryMinus)) ++ "\n" ++
                  show (set leftLens (IntConstant 2) (BinaryTerm (IntConstant 1) (IntConstant 1) BinaryMinus)) ++ "\n" ++
                  show (view leftLens (BinaryTerm (IntConstant 1) (IntConstant 1) BinaryMinus)) ++ "\n" ++
                  show (set rightLens (IntConstant 2) (BinaryTerm (IntConstant 1) (IntConstant 1) BinaryMinus)) ++ "\n" ++
                  show (view rightLens (BinaryTerm (IntConstant 1) (IntConstant 1) BinaryMinus)) ++ "\n" ++
                  show (set centralLens (IntConstant 2) (UnaryTerm (IntConstant 1) UnaryMinus)) ++ "\n" ++
                  show (view centralLens (UnaryTerm (IntConstant 1) UnaryMinus)) ++ "\n" ++
                  show (set tailLens (RCons RNil (2 :: Integer)) (RCons RNil (1 :: Integer))) ++ "\n" ++
                  show (view tailLens (RCons RNil (1 :: Integer))) ++ "\n" ++
                  show (set headLens (2 :: Integer) (RCons RNil (1 :: Integer))) ++ "\n" ++
show (view headLens (RCons RNil (1 :: Integer)))
