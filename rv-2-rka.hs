{-
	Projekt do předmětu FLP
	Převod RV na RKA
	Autor: Bc. Jakub Stejskal
	Soubor: rv-2-rka.hs
	Popis: Skript pro převod zadaného regulárního výrazu na rozšířený konečný automat
-}

{-
	Spuštění - rv-2-rka [volby] [vstup]
	Vstup - pokud není zadáno tak STDIN, jinak název souboru
	Volby - [-r] -> vypíše na STDOUT naparsovaný RV ze vstupu, [-t] - vypíše na STDOUT vytvořený RKA
-}

import System.Environment
import System.Exit
import System.Directory
import Data.List 

-- #######################################################################################################################
-- http://stackoverflow.com/questions/36277160/haskell-reverse-polish-notation-regular-expression-to-expression-tree
-- Převzatý a upravený kód
-- Implementace zpracování RV na strom
data Tree
  = Symbol Char
  | Operand Char Tree Tree
  | EmptyTree
  deriving (Show)

type Stack = [Tree]

step :: Stack -> Char -> Stack
step (r:l:s) '.' = (Operand '.' l r):s
step (r:l:s) '+' = (Operand '+' l r):s
step (l:s) '*' = (Operand '*' l EmptyTree):s
step s c         
    | elem c ['a'..'z']  = (Symbol c):s
    | otherwise  = error "Špatná syntaxe vstupního RV!"

createTree :: String -> Stack
createTree [] = []
createTree input 
    | null xs = tree
    | otherwise = error "Špatná syntaxe vstupního RV!"
    where tree = foldl step [] input
          (x:xs) = tree
-- #######################################################################################################################

-- Datový typ pro pravidla
data Rule = Rule
   { from :: Int
   , symbol :: Char
   , to :: Int }
   deriving (Eq,Show,Ord)

-- Main
main = do
    arguments <- getArgs
    let config = parseArgs arguments
    processRV config

-- Hlavní obslužná funkce pro zpracování RV
processRV :: (String,String) -> IO ()
processRV ("-r",[]) = createRV []
processRV ("-t",[]) = createRKA []
processRV ("-r",file) = createRV file
processRV ("-t",file) = createRKA file

-- Funkce pro zpracování argumentů
parseArgs :: [[Char]] -> (String,String)
parseArgs [] = error "Špatný počet argumentů programu.\nPoužití: rv-2-rka -r/t [vstup]"
parseArgs [option]
    | option == "-r" = ("-r",[])
    | option == "-t" = ("-t",[])
    | otherwise = error "Neznámý argument.\nPoužití: rv-2-rka -r/t [vstup]"
parseArgs [option,file]
    | option == "-r" = ("-r",file)
    | option == "-t" = ("-t",file)
    | otherwise = error "Neznámý argument.\nPoužití: rv-2-rka -r/t [vstup]"
parseArgs _ = error "Špatný formát argumentů.\nPoužití: rv-2-rka -r/t [vstup]"

-- Funkce pro získání vstupu ze STDIN
getInput :: [Char] -> IO String
getInput [] = do 
    input <- getLine
    return input

-- Funkce pro získání vstupu ze souboru
getInput fileName = do
    fileExist <- doesFileExist fileName
    isExist fileExist fileName

-- Funkce pro zjištění existence souboru a načtení vstupu
isExist :: Bool -> FilePath -> IO String
isExist True file = do 
    input <- readFile file
    return input

isExist False file =
    error "Soubor neexistuje"

-- ******************************************************************************************************************
-- Funkce pro převedení vstupu na standartní RV
-- Přepínač -r
createRV :: [Char] -> IO()
createRV fileName = do
    input <- getInput fileName
    --putStrLn ("Vstupní RV v postfixovém tvaru: "++ input)
    let tree = headTree (createTree input)
    --putStrLn ("Vnitřní reprezentace RV: "++ show tree)   -- vypíše vnitřní reprezentaci v podobě stromu
    let rv = parseTree2RV tree
    putStrLn ("Vstupní RV ve standartním tvaru: "++ clearRV rv)     -- vypíše vstupní RV v klasické podobě

-- Funkce pro ošetření prázdnosti stromu
headTree :: [Tree] -> Tree
headTree tree
    | null tree = EmptyTree
    | otherwise = head tree

-- Funkce pro převedení stromu RV na klasický RV
parseTree2RV :: Tree -> [Char]
parseTree2RV EmptyTree = []
parseTree2RV (Symbol c) = c:[]
parseTree2RV (Operand f l r)
    | f == '.' = parseTree2RV l ++ parseTree2RV r
    | f == '+' = "(" ++ parseTree2RV l ++ (f:[]) ++ parseTree2RV r ++ ")"
    | otherwise = parseTree2RV l ++ (f:[]) ++ parseTree2RV r

-- Funkce pro odstranění závorek, které obklopují celý výraz
clearRV :: [Char] -> [Char]
clearRV [] = []
clearRV expresion
    | head expresion == '(' && last expresion == ')' = tail (init expresion)
    | otherwise = expresion

-- ******************************************************************************************************************
-- Funkce pro převedení vstupu na RKA
-- Přepínač -t
createRKA :: [Char] -> IO()
createRKA fileName = do
    input <- getInput fileName
    --putStrLn ("Vnitřní reprezentace RV: " ++ (show (createTree input)))   -- Výpis vnitřní reprezentace, pouze pro kontrolu
    let tree = headTree (createTree input)
    let rka = parseTree2RKA [] tree 1
    printRKA rka

-- Funkce pro vypsání výsledného RKA
printRKA :: [Rule] -> IO ()
printRKA rules = do
    putStrLn (show' (sort states))
    print (minimum states)
    print (maximum states)
    putStrLn (parseRules (sort  rules))
    where states = getStates rules
          show' = intercalate "," . map show                    -- Funkce pro rozdělení seznamu na jeden řetězec, kde prvky jsou odděleny čárkou
          getStates [] = [1]                                    -- Funkce pro získání seznamu stavů
          getStates rules = foldl stateStep [] rules            -- Funkce pro získání seznamu stavů

-- Funkce pro zpracování pravidel pomcí foldl
stateStep :: [Int] -> Rule -> [Int]
stateStep states rule
    | (elem x states) && (notElem z states) = z:states
    | (elem z states) && (notElem x states) = x:states
    | (elem x states) && (elem z states) = states
    | otherwise = x:z:states
    where x = from rule
          z = to rule

-- Funkce pro vypsání jednotlivých stavů automatu
parseRules :: [Rule] -> String
parseRules [] = ""
parseRules [x] = (show (from x) ++ "," ++ parseEps (symbol x) ++ "," ++ show (to x))
parseRules (x:xs) = show (from x) ++ "," ++ parseEps (symbol x) ++ "," ++ show (to x) ++ "\n" ++ parseRules xs

-- Funkce pro odmazání mezer při epsilon přechodech
parseEps :: Char -> String
parseEps ' ' = ""
parseEps x = x:[]

-- Funkce pro převedení stromu RV na RKA
parseTree2RKA :: [Rule] -> Tree -> Int -> [Rule]
parseTree2RKA (rules) EmptyTree actualState = (rules)
-- Vzor funkce pro zpracování .
parseTree2RKA (rules) (Operand '.' l r) actualState = parseTree2RKA leftTree r newActualState
    where leftTree = parseTree2RKA rules l actualState
          newActualState = to (last leftTree)

-- TODO PŘEDĚLAT!!!! TODO
-- Vzor funkce pro zracování +
parseTree2RKA (rules) (Operand '+' l r) actualState = finalLeftTree ++ finalRightTree
    where leftTree = (Rule actualState ' ' (actualState+1)):[] ++ parseTree2RKA (rules) l (actualState+1)
          newActualState1 = to (last leftTree)
          rightTree = (Rule actualState ' ' (newActualState1+1)):[] ++ parseTree2RKA (rules) r (newActualState1+1)
          newActualState2 = to (last rightTree)
          finalLeftTree = leftTree ++ (Rule newActualState1 ' ' (newActualState2+1)):[]
          finalRightTree = rightTree ++ (Rule newActualState2 ' ' (newActualState2+1)):[]

-- Vzor funkce pro zracování *
parseTree2RKA (rules) (Operand '*' l r) actualState = finalTree
    where leftTree = (Rule actualState ' ' (actualState+1)):[] ++ parseTree2RKA (rules) l (actualState+1)
          newActualState = to (last leftTree) 
          finalTree = leftTree ++ (Rule newActualState ' ' (newActualState+1)):[] ++ (Rule newActualState ' ' (actualState+1)):[] ++ (Rule actualState ' ' (newActualState+1)):[]

-- Vzor funkce pro zpracování symbolů
parseTree2RKA (rules) (Symbol c) actualState = rules ++ (Rule actualState c (actualState+1)):[]
   

