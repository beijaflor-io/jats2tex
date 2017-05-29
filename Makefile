tag=$(shell git describe --tags)

build: FORCE
	stack build

full-release: FORCE
	make release
	docker build -t jats2tex:$(tag) .
	heroku container:push

release: FORCE
	echo $(tag)
	./bin/stack-fpm jats2tex $(tag)
	github-release release -u beijaflor-io -r jats2tex -t $(tag) -n $(tag)
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
