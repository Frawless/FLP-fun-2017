{-
	Projekt do předmětu FLP
	Převod RV na RKA
	Autor: Bc. Jakub Stejskal
	Soubor: Config.hs
	Popis: Souborj obsahující funkcí na parsování vstupních argumentů.
-}

module Parser.Config
    where

import Type.Config

-- Funkce pro zpracování argumentů
parseArguments :: [String] -> Either String Config
parseArguments [] = Left $ "Špatný počet argumentů programu.\nPoužití: rv-2-rka -r/t [vstup]"
parseArguments [option]
    | option == "-r" = Right $ Config CreateRV []
    | option == "-t" = Right $ Config CreateRKA []
    | otherwise = Left $ "Neznámý argument.\nPoužití: rv-2-rka -r/t [vstup]"
parseArguments [option,file]
    | option == "-r" = Right $ Config CreateRV file
    | option == "-t" = Right $ Config CreateRKA file
    | otherwise = Left $ "Neznámý argument.\nPoužití: rv-2-rka -r/t [vstup]"
parseArguments _ = Left $ "Špatný formát argumentů.\nPoužití: rv-2-rka -r/t [vstup]"
