FROM ocaml/opam:alpine as build
RUN sudo apk add --update openssl-dev pkgconfig libffi linux-headers build-base patch tar ca-certificates git rsync curl sudo bash libx11-dev nano coreutils xz ncurses
COPY boot /home/opam/src/boot
COPY Makefile /home/opam/src/Makefile
RUN sudo chown -R opam /home/opam/src
WORKDIR /home/opam/src
RUN make boot
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
RUN make
FROM alpine
COPY --from=build /home/opam/src/_build/install/default/bin/eeww /usr/bin/eeww
