FROM heroku/heroku:16

RUN apt-get update
RUN apt-get install -y libicu-dev libgmp-dev

COPY ./.stack-fpm/linux/usr/local/bin/jats2tex /usr/local/bin/jats2tex
COPY ./.stack-fpm/linux/usr/local/bin/jats2tex-web /usr/local/bin/jats2tex-web
COPY ./.stack-fpm/linux/usr/local/var /usr/local/var
COPY ./.stack-fpm/linux/usr/local/etc /usr/local/etc

WORKDIR /usr/local/etc/jats2tex
CMD jats2tex-web /usr/local/etc/jats2tex/config/settings.yml
