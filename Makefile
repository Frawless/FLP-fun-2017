# Projekt do předmětu FLP
# Převod RV na RKA
# Autor: Bc. Jakub Stejskal
# Soubor: Makefile
# Popis: Skript pro přeložení vytvořené aplikace v jazyce Haskell


all:
	ghc --make rv-2-rka.hs

run:
	./rv-2-rka -r

rut:
	./rv-2-rka -t

clean:
	rm -rf rv-2-rka *.o *.hi
