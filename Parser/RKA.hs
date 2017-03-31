{-
	Projekt do předmětu FLP
	Převod RV na RKA
	Autor: Bc. Jakub Stejskal
	Soubor: Parser/RKA.hs
	Popis: Soubor obsahující hlavní logiku pro převod RV na strom a jeho následné zpracování na RKA/RV.
-}

module Parser.RKA
    where

import Data.List 
import Data.Char
import Text.ParserCombinators.ReadP

import Type.Config
import Type.RKA

-- Startovací stav
firstState = 1
comma = ","
newLine = "\n"

-- ******************************************************************************************************************
-- Funkce pro vytvoření RV ze stromu (postfix notace!)
createPostFix input = 
    case tree of
        Left string -> putStrLn string
        Right stack -> putStrLn $ parseTree2RV $ headTree stack
    where   tree = createTree input
            -- Funkce pro převedení stromu RV na klasický RV
            parseTree2RV EmptyTree = []
            parseTree2RV (Symbol c) = c:[]
            parseTree2RV (Operand f l r)
                | f == '.' = parseTree2RV l ++ parseTree2RV r ++ op
                | f == '+' = parseTree2RV l ++ parseTree2RV r ++ op
                | otherwise = parseTree2RV l ++ parseTree2RV r ++ op
                where op = f:[]

-- ******************************************************************************************************************
-- Funkce pro převedení vstupu na RKA
-- Přepínač -t
createRKA :: String -> IO()
createRKA input = do
    --putStrLn ("Vnitřní reprezentace RV: " ++ (show (createTree input)))   -- Výpis vnitřní reprezentace, pouze pro kontrolu
    case tree of
        Left string -> putStrLn string
        Right stack -> printRKA $ rkaParser $ parseTree2RKA [] (headTree stack) firstState
    where tree = createTree input

-- Funkce pro vytvoření konečného automatu ze získaných pravidel
rkaParser :: [TRule] -> TRka
rkaParser rules =
    TRka states (startState states) (endState states) $ emptyRV rules
    where states = sort $ foldl stateStep [] $ emptyRV rules
          emptyRV rules = case rules of                         -- Možnost prázdného vstupu
                            [] -> [TRule firstState ' ' firstState]
                            any -> any

-- Funkce pro získání počátečního stavu                         
startState states = minimum states
-- Funkce pro získání koncového stavu                         
endState states = maximum states

-- Alternativní funkce pro převod seznamu na string
show' :: Show a => [a] -> String
show' = intercalate "," . map show

-- Funkce pro výpis konečného automatu
printRKA :: TRka -> IO()
printRKA rka = do
    putStrLn $ show' $ states rka
    putStrLn $ show $ start rka
    putStrLn $ show $ end rka
    putStrLn $ showRules $ sort $ rules rka

-- Funkce pro zpracování pravidel pomcí foldl
stateStep :: [TState] -> TRule -> [TState]
stateStep states rule
    | elem x states && notElem z states = z:states
    | elem z states && notElem x states = x:states
    | elem x states && elem z states = states
    | x == z = x:states
    | otherwise = x:z:states
    where x = from rule
          z = to rule

-- Funkce pro vypsání jednotlivých stavů automatu
showRules :: [TRule] -> String
showRules [] = ""
showRules [x] = show (from x) ++ comma ++ parseEps (symbol x) ++ comma ++ show (to x)
showRules (x:xs) = show (from x) ++ comma ++ parseEps (symbol x) ++ comma ++ show (to x) ++ newLine ++ showRules xs

-- Funkce pro odmazání mezer při epsilon přechodech
parseEps :: Char -> String
parseEps ' ' = ""
parseEps x = x:[]

-- Funkce pro převedení stromu RV na RKA
parseTree2RKA :: [TRule] -> Tree -> Int -> [TRule]
parseTree2RKA rules EmptyTree actState = rules
-- Vzor funkce pro zpracování .
parseTree2RKA rules (Operand '.' l r) actState = leftTree ++ (parseTree2RKA rules r nextState)
    where leftTree = parseTree2RKA rules l actState
          nextState = to $ last leftTree

-- Vzor funkce pro zracování +
parseTree2RKA rules (Operand '+' l r) actState = finalUpTree ++ finalDownTree
    where upTree = (TRule actState ' ' (actState+1)):[] ++ parseTree2RKA rules l (actState+1)
          nextState = to $ last upTree
          downTree = (TRule actState ' ' (nextState+1)):[] ++ parseTree2RKA rules r (nextState+1)
          finalState = to (last downTree)
          finalUpTree = upTree ++ (TRule nextState ' ' (finalState+1)):[]
          finalDownTree = downTree ++ (TRule finalState ' ' (finalState+1)):[]

-- Vzor funkce pro zracování *
parseTree2RKA rules (Operand '*' l r) actState = createdRules 
    where leftTree = (TRule actState ' ' (actState+1)):[] ++ parseTree2RKA rules l (actState+1)
          nextState = to $ last leftTree
          createdRules = leftTree ++ (TRule nextState ' ' (nextState+1)):[] ++ (TRule nextState ' ' (actState+1)):[] ++ (TRule actState ' ' (nextState+1)):[]

-- Vzor funkce pro zpracování symbolů
parseTree2RKA rules (Symbol c) actState = rules ++ (TRule actState c (actState+1)):[]
-- ******************************************************************************************************************
