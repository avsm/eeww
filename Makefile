.PHONY: clean doc all

all:
	dune build

doc:
	dune build @doc

clean:
	dune clean
