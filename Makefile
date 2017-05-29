build:
	stack build

build-pdf: FORCE
	pandoc ./README.md -o ./jats2tex.pdf

build-osx: FORCE
	stack build --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include

example: FORCE
	stack run -- -- ./example1.xml ./example1.tex && docker run -it -v `pwd`:/tmp latexindent /app/latexindent.pl /tmp/example1.tex 

FORCE:
