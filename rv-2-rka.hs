{-# LANGUAGE RecordWildCards #-}
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

import Control.Applicative
import System.Environment
import System.IO

import Parser.RKA
import Parser.Config
import Type.RKA
import Type.Config


-- Main
main = do
    arguments <- parseArguments <$> getArgs
    -- Zpracování zpracovaných argumentů
    case arguments of
        Left err -> putStrLn err
        Right args -> handleArgs args

-- Získání argumentů ze struktury a spuštění hlavní části programu
handleArgs :: Config -> IO ()
handleArgs Config{..} = processRV action regularExp
        --CreateRKA -> createRKA regularExp

-- Hlavní obslužná funkce pro zpracování vstupního RV
processRV :: CAction -> String -> IO()
processRV action [] = do
    input <- getLine
    case action of
        CreateRV -> createPostFix input
        CreateRKA -> createRKA input

processRV action fileName = do
    fileExist <- doesFileExist fileName
    case fileExist of
        False -> putStrLn "Soubor neexistuje!"
        True -> parseRV action fileName
    where parseRV action fileName = do
            input <- readFile fileName
            case action of
                CreateRV -> createPostFix input
                CreateRKA -> createRKA input
-- ******************************************************************************************************************
