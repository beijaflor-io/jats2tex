# USE ALPINE LINUX
FROM alpine
RUN apk update
# INSTALL BASIC DEV TOOLS, GHC, GMP & ZLIB
RUN echo "https://s3-us-west-2.amazonaws.com/alpine-ghc/8.0" >> /etc/apk/repositories
ADD https://raw.githubusercontent.com/mitchty/alpine-ghc/master/mitch.tishmack%40gmail.com-55881c97.rsa.pub \
    /etc/apk/keys/mitch.tishmack@gmail.com-55881c97.rsa.pub
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc gmp-dev zlib-dev
# GRAB A RECENT BINARY OF STACK
ADD https://s3.amazonaws.com/static-stack/stack-1.1.2-x86_64 /usr/local/bin/stack
RUN chmod 755 /usr/local/bin/stack

# FIX https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/640734
WORKDIR /usr/lib/gcc/x86_64-alpine-linux-musl/6.2.1
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
RUN apk add icu-dev icu-libs icu

# COMPILE jats2tex
ADD ./ /usr/src/jats2tex
WORKDIR /usr/src/jats2tex
RUN stack --local-bin-path /sbin install --test --ghc-options '-optl-static -fPIC -optc-Os'

# COMPRESS WITH UPX
ADD https://github.com/lalyos/docker-upx/releases/download/v3.91/upx /usr/local/bin/upx
RUN chmod 755 /usr/local/bin/upx
RUN upx --best --ultra-brute /sbin/jats2tex

# SHOW INFORMATION ABOUT jats2tex
RUN ldd /sbin/jats2tex || true
RUN du -hs /sbin/jats2tex
