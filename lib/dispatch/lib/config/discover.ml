module C = Configurator.V1

let () =
  C.main ~name:"foo" (fun _c ->
      let conf : C.Pkg_config.package_conf =
        { libs = [ "-lobjc" ]; cflags = [] }
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
