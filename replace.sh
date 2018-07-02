

sed -i 's/\\affil\[\w\+\]/\L&/g' *.tex
sed -i 's/\\author\[\w\+\]/\L&/g' *.tex
sed -i ':a;N;$!ba;s/\n\\textsuperscript/\\textsuperscript/g' *.tex
sed -i ':a;N;$!ba;s/\n\\footnote/\\footnote/g' *.tex
sed -i ':a;N;$!ba;s/\n\]/\]/g' *.tex
sed -i ':a;N;$!ba;s/\n}/}/g' *.tex
sed -i 's/\ \\footnote/\\footnote/g' *.tex
sed -i 's/\ ,/,/g' *.tex
sed -i 's/e-mail/\\mbox{e-mail}/g' *.tex
sed -i ':a;N;$!ba;s/\n,/,/g' *.tex
sed -i ':a;N;$!ba;s/\n\././g' *.tex
sed -i 's/\([A-ZÁ-Ź][A-ZÁ-Ź][A-ZÁ-Ź]\+\)/\\textsc{\L\1}/g' *.tex
sed -i 's/\(\\item\[\w\+\]\)\ \+\./\1/g' *.tex
sed -i 's/\(\\item\[\w\+\]\) \+[0-9]\+\(\.\|\)/\1/g' *.tex
sed -i 's/\ [0-9]\+\(\~\\ref\)/\~\\ref/g' *.tex
sed -i ':a;N;$!ba;s/\n[0-9]\+\(\~\\ref\)/\~\\ref/g' *.tex

sed -i 's/\\textsuperscript{\\textsuperscript{\[}\([0-9]\+\)\\textsuperscript{\]}}/\\textsuperscript{\[\1\]}/g' *.tex
sed -i 's/\ \\textsuperscript/\\textsuperscript/g' *.tex
sed -i 's/\ \\textsuperscript/\\textsuperscript/g' *.tex

sed -i 's/\\author\[\\textsuperscript/\\author\[/g' *.tex
sed -i 's/\\author\[\]/\\author/g' *.tex
sed -i 's/\\affil\[\]/\\affil/g' *.tex
