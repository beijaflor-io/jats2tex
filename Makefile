version=$(shell cat ./package.yaml | kv-formats -i yaml -o json | jq '.version')
tag=$(shell git describe --tags)

build: build-frontend FORCE
	stack build

build-frontend: FORCE
	cd ./jats2tex-web && npm run build

build-manpages: FORCE
	for i in ./docs/man/*.md; do \
		echo $$i; \
		echo `dirname $$i`; \
		pandoc -s $$i -t man -o `dirname $$i`/`basename $$i .md`.man; \
	done

build-linux: build-frontend FORCE
	stack docker pull
	stack build --ghc-options=-O2 --docker # --ghc-options="-optc-Os -optl-static -fPIC"

package: FORCE
	./bin/stack-fpm jats2tex $(version)

full-release: FORCE
	make release
	make deploy

deploy: FORCE
	make build-image
	heroku container:push

build-image: FORCE
	docker build -t jats2tex:$(tag) .

push-image:
	docker tag jats2tex:$(tag) beijaflorio/jats2tex:$(tag)
	docker tag jats2tex:$(tag) beijaflorio/jats2tex:latest
	docker push beijaflorio/jats2tex:latest
	docker push beijaflorio/jats2tex:$(tag)

release: FORCE
	echo $(tag)
	make package
	github-release release -u beijaflor-io -r jats2tex -t $(tag) -n $(tag)
	make upload

upload: FORCE
	for i in dist/*$(version)*; do \
		echo $$i; \
		github-release upload -u beijaflor-io -r jats2tex -t $(tag) -n $$i -f $$i -l $$i; \
	done

build-pdf: FORCE
	pandoc ./README.md -o ./jats2tex.pdf

build-osx: build-frontend FORCE
	stack build --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include

example-tables: FORCE
	stack build # --executable-profiling --library-profiling
	rm -f ./example-input-tables.{tex,pdf,log,aux}
	stack exec jats2tex -- -t ./standalone-tables.yaml ./example-input-tables.xml --
	lualatex ./example-input-tables.tex

example: FORCE
	stack build
	rm -f ./examples/JATS/Original\ article/0065-1737-azm-31-03-0367/0065-1737-azm-31-03-0367.{dvi,log,pdf,aux}
	stack exec jats2tex -- ./examples/JATS/Original\ article/0065-1737-azm-31-03-0367/0065-1737-azm-31-03-0367.xml
	cd ./examples/JATS/Original\ article/0065-1737-azm-31-03-0367/ && lualatex ./*.tex

example-profile: FORCE
	stack build --executable-profiling --library-profiling --ghc-options=-rtsopts\ -O2 --verbose
	rm -f ./examples/JATS/Original\ article/0065-1737-azm-31-03-0367/0065-1737-azm-31-03-0367.{dvi,log,pdf,aux}
	stack exec jats2tex -- +RTS -pa -h -RTS ./examples/JATS/Original\ article/0065-1737-azm-31-03-0367/0065-1737-azm-31-03-0367.xml
	cd ./examples/JATS/Original\ article/0065-1737-azm-31-03-0367/ && lualatex ./*.tex
	hp2ps -e8in -c ./jats2tex.hp
	ps2pdf14 ./jats2tex.ps

run-web: FORCE
	cd ./jats2tex-web && stack exec jats2tex-web ./config/settings.yml

FORCE:
