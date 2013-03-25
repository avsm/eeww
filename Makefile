OBUILDOPTS=--debug+
#CONFOPTS=--enable-library-bytecode --enable-executable-bytecode
PKGNAME=tuntap

.PHONY: configure build install clean uninstall

all: build

configure:
	obuild $(OBUILDOPTS) configure $(CONFOPTS)

build: configure
	obuild $(OBUILDOPTS) build

install: build
	ocamlfind remove $(PKGNAME)
	ocamlfind install $(PKGNAME) dist/build/lib-$(PKGNAME)/*.{a,so,cma,cmxa,cmi} lib/META lib/$(PKGNAME).mli

clean:
	obuild clean

uninstall:
	ocamlfind remove $(PKGNAME)
