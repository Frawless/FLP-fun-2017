# Projekt do předmětu FLP
# Převod RV na RKA
# Autor: Bc. Jakub Stejskal
# Soubor: Makefile
# Popis: Skript pro přeložení vytvořené aplikace v jazyce Haskell

LOGIN = flp-fun-xstejs24
PACK = Parser/*.hs Type/*.hs *.hs Makefile Tests README

all:
	ghc --make rv-2-rka.hs

run:
	./rv-2-rka -r

rut:
	./rv-2-rka -t

clean:
	rm -rf rv-2-rka *.o *.hi Parser/*.o Parser/*.hi Type/*.o Type/*.hi Tests/diff Tests/output

test:
	./Tests/test.sh

pack:
	rm -f $(LOGIN).zip
	make clean
	zip -r $(LOGIN).zip $(PACK)
