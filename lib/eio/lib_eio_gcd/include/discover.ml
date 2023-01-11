module C = Configurator.V1

(* Borrowed from liburing *)
let () =
  C.main ~name:"discover" (fun c ->
      let defs =
        C.C_define.import c ~includes:["fcntl.h"]
          C.C_define.Type.[
            "O_RDONLY", Int;
            "O_WRONLY", Int;
            "O_RDWR", Int;
            "O_CREAT", Int;
            "O_EXCL", Int;
            "O_TRUNC", Int;
            "O_APPEND", Int;
            "O_NONBLOCK", Int;
            "O_DIRECTORY", Int;
            "O_NOFOLLOW", Int;
            "O_CLOEXEC", Int;
          ]
        |> List.map (function
            | name, C.C_define.Value.Int v ->
              let name = 
                match name with
                | "sizeof(struct iovec)" -> "sizeof_iovec"
                | "sizeof(struct __kernel_timespec)" -> "sizeof_kernel_timespec"
                | nm -> nm 
              in
              Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
            | _ -> assert false
          )
      in
      C.Flags.write_lines "config.ml" defs
    )