all:
	ocaml pkg/pkg.ml build -n mirage-flow -q
	ocaml pkg/pkg.ml build -n mirage-flow-lwt
	ocaml pkg/pkg.ml build -n mirage-flow-unix --tests true -q
	ocaml pkg/pkg.ml test

clean:
	ocaml pkg/pkg.ml clean
