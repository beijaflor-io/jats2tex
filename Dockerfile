FROM heroku/heroku:16

RUN apt-get update
RUN apt-get install -y libicu-dev libgmp-dev

COPY ./.stack-fpm/linux/usr/local/bin/jats2tex /usr/local/bin/jats2tex
COPY ./.stack-fpm/linux/usr/local/bin/jats2tex-web /usr/local/bin/jats2tex-web

CMD jats2tex-web
