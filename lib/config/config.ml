module C = Configurator.V1
module SMap = Map.Make (String)

let kqueue_available vars =
  List.exists
    (fun (_, v) ->
      match v with
      | C.C_define.Value.Switch true -> true
      | _ -> false)
    vars
;;

let () =
  C.main ~name:"kqueue.conf" (fun conf ->
      let system =
        C.C_define.import
          conf
          ~includes:[ "caml/config.h" ]
          [ "ARCH_SIXTYFOUR", C.C_define.Type.Switch ]
      in
      let operating_systems =
        C.C_define.import
          conf
          ~includes:[]
          [ "__APPLE__", C.C_define.Type.Switch
          ; "__FreeBSD__", C.C_define.Type.Switch
          ; "__OpenBSD__", C.C_define.Type.Switch
          ; "__DragonFly__", C.C_define.Type.Switch
          ; "__NetBSD__", C.C_define.Type.Switch
          ]
      in
      let vars =
        [ "KQUEUE_AVAILABLE", C.C_define.Value.Switch (kqueue_available operating_systems)
        ; "FREEBSD", List.assoc "__FreeBSD__" operating_systems
        ; "OPENBSD", List.assoc "__OpenBSD__" operating_systems
        ; "DRAGONFLY", List.assoc "__DragonFly__" operating_systems
        ; "NETBSD", List.assoc "__NetBSD__" operating_systems
        ; "KQUEUE_ML_ARCH_SIXTYFOUR", List.assoc "ARCH_SIXTYFOUR" system
        ]
      in
      C.C_define.gen_header_file conf ~fname:"config.h" vars)
;;
