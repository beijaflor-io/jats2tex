TAG=0034-8910-rsp-47-04-0791

all:
	jats2tex $(TAG).xml -t template.yaml
	sh replace.sh
	cat $(TAG).tex

2:
	jats2tex $(TAG).xml -t template2.yaml
	cat $(TAG).tex

debug:
	JATS2TEX_DEBUG=1 jats2tex -w 0 $(TAG).xml -t template.yaml

sublime:
	subl3 -n . $(TAG).xml $(TAG).tex template.yaml Makefile

pdf:
	latexmk -xelatex $(TAG).tex
	evince $(TAG).pdf

xelatex:
	xelatex $(TAG).tex


pdf-modelo:
	evince MODELO.pdf

git:
	-git add .
	-git commit -m "General edition"

clean:
	-rm *.aux *.log *.xdv *.out 
update:
	-cp template.yaml ..
	-cp template.yaml ~/Dropbox/SCIELO/jats2tex/default.yaml
	-cp template.yaml /home/editorial/Dropbox_OUT/SCIELO/SPS+TeX/SCIELO_RENATA/scielo-sp-amostra/artigos/
upgrade:
	-cp ../template.yaml .

