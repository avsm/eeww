module C = Configurator.V1

let () =
  C.main ~name:"foo" (fun c ->
      let conf : C.Pkg_config.package_conf =
        {
          libs =
            [
              "-L" ^ C.ocaml_config_var_exn c "standard_library";
              "-lthreadsnat";
              "-lunix";
              "-ObjC";
            ];
          cflags = [];
        }
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
