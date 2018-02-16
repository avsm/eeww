all:
	jbuilder build @install @runtest-pbkdf

.PHONY: clean
clean:
	rm -rf _build
