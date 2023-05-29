.PHONY: clean distclean all boot

all: _obj/bin/dune
	@env PATH="`pwd`/_obj/bin:$$PATH" dune build @install #lib/meio/src/bin/meio.exe lib/letsencrypt/bin/oacmel.exe 

boot: _obj/bin/dune
	@:
	
_obj/bin/dune: _obj/bin/.stamp
	sh ./boot/build.sh native

_obj/.stamp:
	mkdir -p _obj
	@touch $@

_obj/bin/.stamp: _obj/.stamp
	mkdir -p _obj/bin
	@touch $@

solve: boot
	sh ./boot/solve.sh

clean:
	rm -rf _build

distclean: clean
	rm -rf _obj
