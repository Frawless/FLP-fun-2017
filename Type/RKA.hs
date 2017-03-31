{-
	Projekt do předmětu FLP
	Převod RV na RKA
	Autor: Bc. Jakub Stejskal
	Soubor: Type/RKA.hs
	Popis: Soubor obsahující vytvořené datové typy a funkce pro zpracování RV do stromu.
-}

module Type.RKA
    where

import Data.Foldable

-- Strom pro RV
data Tree
  = Symbol Char
  | Operand Char Tree Tree
  | EmptyTree
  deriving (Show)

-- Zásobník
type Stack = [Tree]

-- Datový typ pro RKA
data TRka = TRka
    { states :: [TState]
    , start :: TState
    , end :: TState
    , rules :: [TRule]
    }
    deriving (Eq, Show)

-- Datový typ pro pravidla
data TRule = TRule
   { from :: TState
   , symbol :: Char
   , to :: TState }
   deriving (Eq,Show,Ord)

-- Typ pro stav
type TState = Int

-- Funkce pro ošetření prázdnosti stromu
headTree :: [Tree] -> Tree
headTree tree
    | null tree = EmptyTree
    | otherwise = head tree

-- #######################################################################################################################
-- http://stackoverflow.com/questions/36277160/haskell-reverse-polish-notation-regular-expression-to-expression-tree
-- Převzatý a upravený kód
-- Implementace zpracování RV na strom
step :: Stack -> Char -> Stack
step (r:l:s) '.' = (Operand '.' l r):s
step (r:l:s) '+' = (Operand '+' l r):s
step (l:s) '*' = (Operand '*' l EmptyTree):s
step s c         
    | elem c ['a'..'z']  = (Symbol c):s
    | otherwise  = error "Špatná syntaxe vstupního RV!"

createTree :: String -> Either String Stack
createTree [] = Right []
createTree input 
    | null xs = Right tree
    | otherwise = Left "Špatná syntaxe vstupního RV!"
    where tree = foldl step [] input
          (x:xs) = tree

-- #######################################################################################################################
