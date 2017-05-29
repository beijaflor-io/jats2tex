for i in dist/*; do github-release upload -u beijaflor-io -r jats2tex -t v0.1.0.0 -n $i -f $i -l $i; done
