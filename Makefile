OBUILDOPTS="--debug+"
PACKAGE_NAME=tuntap

.PHONY: configure build install clean uninstall

all: build

configure:
	obuild $(OBUILDOPTS) configure

build: configure
	obuild $(OBUILDOPTS) build

install: build
	ocamlfind remove $(PACKAGE_NAME)
	ocamlfind install $(PACKAGE_NAME) dist/build/lib-$(PACKAGE_NAME)/$(PACKAGE_NAME).{a,cma,cmxa} dist/build/lib-$(PACKAGE_NAME)/tuntap.cmi lib/META

clean:
	obuild clean

uninstall:
	ocamlfind remove $(PACKAGE_NAME)
