.PHONY: all
all:
	@dune build @install @examples

.PHONY: run-examples
run-examples:
	dune exec examples/filtering.exe < examples/filtering.json

.PHONY: install
install:
	@dune install

.PHONY: uninstall
uninstall:
	@dune uninstall

.PHONY: bench
bench:
	@dune build --display=quiet @bench-generic-sexp --force
	@dune build --display=quiet @bench-buffer-sexp --force

.PHONY: bench-local
bench-local:
	@dune build @bench --force

.PHONY: clean
clean:
	@dune clean

.PHONY: test
test:
	@dune runtest --force

