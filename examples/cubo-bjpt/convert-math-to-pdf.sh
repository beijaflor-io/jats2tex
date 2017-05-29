#!/bin/bash
# seleciona todos os arquivos tex da pasta mathml e converte para pdf

mathFolder="mathml"

# sty
cp pmml-new.sty "$mathFolder/pmml-new.sty"

cd $mathFolder

for file in `ls *.tex`
do
	imagename=`echo $file | sed -r "s/\.tex$/\.png/gi"`
	pdfname=`echo $file | sed -r "s/\.tex$/\.pdf/gi"`

	# compila a formula para pdf
	latexmk -quiet -interaction=nonstopmode -xelatex "$file"
	
done

cd ..