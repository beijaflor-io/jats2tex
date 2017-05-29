tag=$(shell git describe --tags)

build: FORCE
	stack build

release: FORCE
	echo $(tag)
	stack fpm
	for i in dist/*; \
		do github-release upload -u beijaflor-io -r jats2tex -t $(tag) -n $$i -f $$i -l $$i; \
	done

build-pdf: FORCE
	pandoc ./README.md -o ./jats2tex.pdf

build-osx: FORCE
	stack build --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include

example: FORCE
	stack run -- -- ./example1.xml ./example1.tex && docker run -it -v `pwd`:/tmp latexindent /app/latexindent.pl /tmp/example1.tex 

FORCE:
