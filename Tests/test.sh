#!/bin/bash

# Projekt do předmětu FLP
# Převod RV na RKA
# Autor: Bc. Jakub Stejskal
# Soubor: test.hs
# Popis: Skript pro testování vytvořeného převodníku

red='\033[1;31m'
green='\033[1;32m'
NC='\033[0m'

INPUT=input/test
OUTPUT=output/output
REF=ref_out/ref_out

# Odstranění staré binárky a předchozích výstupů
rm -r Tests/diff
rm -r Tests/output

# cd ../
# Ověření existence makefile a rv-2-rka
if [ ! -e Makefile ]; then
	echo -e "${red}Nenalezeno Makefile!"
	exit 1
fi

if [ ! -e rv-2-rka.hs ]; then
	echo -e "${red}Nenalezen soubor 'rv-2-rka.hs'!"
	exit 1
fi


# Překlad
make clean
make

# Kontrola překladu
if [[ $? -ne 0 ]] ; then
	echo -e "${red}Nelze přeložit!"
    exit 1
fi

# Vstup do podsložky s testy
cd Tests
mkdir diff
mkdir output

# Testování
x=$(ls input/ -1 | wc -l)	# Počet testů
for ((i=1; i <= x; i++))
do
	./../rv-2-rka -t ${INPUT}$i > ${OUTPUT}$i
	diff -u ${REF}$i ${OUTPUT}$i > diff/diff$i
	if [ $? = 0 ]
		then
			echo -e "${green}Test ${i} PASS! -> in: $(cat ${INPUT}$i)"
		else
			echo -e "${red}Test ${i} FAIL -> in: $(cat ${INPUT}$i) - check diff/diff$i"
		fi
		echo -e "${NC}----------------------"
done

# Progress bar
# echo -ne '#####                     (33%)\r'
# sleep 1
# echo -ne '#############             (66%)\r'
# sleep 1
# echo -ne '#######################   (100%)\r'
# echo -ne '\n'
