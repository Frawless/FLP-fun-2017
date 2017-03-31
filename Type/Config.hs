{-
	Projekt do předmětu FLP
	Převod RV na RKA
	Autor: Bc. Jakub Stejskal
	Soubor: Type/Config.hs
	Popis: Soubor obsahující konfigurační struktury pro vyvolání správně akce.
-}

module Type.Config
    where

-- Typ pro vyvolání správně akce
data CAction
    = CreateRV
    | CreateRKA
  deriving (Show)

-- Struktura pro akci a vstup
data Config = Config
    { action :: CAction
    , regularExp :: FilePath
    }
  deriving (Show)
