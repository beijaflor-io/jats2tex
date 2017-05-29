all:
	-rm LIVRO.pdf
	make clean
	latexmk -xelatex LIVRO.tex
scielo:
	sh convert-math-to-pdf.sh
	latexmk -quiet -interaction=nonstopmode -xelatex LIVRO.tex
	-rm LIVRO.pdf
	latexmk -quiet -interaction=nonstopmode -xelatex LIVRO.tex
clean:
	-rm *.aux *.fls *.fdb_latexmk *.log *.bib *.blg *.bbl *.ltx *.run.xml
debug:
	-rm LIVRO.pdf
	make clean
	latexmk -xelatex LIVRO.tex
	cp LIVRO.pdf ~/vmboxshared/LIVRO.pdf
	#evince LIVRO.pdf