FROM alpine as build
RUN apk add --update openssl-dev pkgconfig libffi linux-headers build-base patch tar ca-certificates git rsync curl bash libx11-dev nano coreutils xz ncurses cmake zlib-dev gmp-dev capnproto-dev
COPY boot /src/boot
COPY Makefile /src/Makefile
WORKDIR /src
RUN make boot
COPY . /src
ENV DUNE_PROFILE=release
RUN make
FROM alpine
COPY --from=build /src/_build/install/default/bin/eeww /usr/bin/eeww
