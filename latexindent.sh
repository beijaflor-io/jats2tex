#!/bin/bash -e
latexindent () {
  docker run -it -v `pwd`:/tmp latexindent /app/latexindent.pl /tmp/$1
}

latexindent $1
