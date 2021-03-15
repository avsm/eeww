let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let () =
  let is_64bit =
    let usage () = invalid_arg "%s --sixtyfour (true|false)" Sys.argv.(0) in
    match Sys.argv with
    | [|_; "--sixtyfour"; ("true" | "!true")|] -> true
    | [|_; "--sixtyfour"; ("false" | "!false")|] -> false
    | [|_; "--sixtyfour"; v |] -> invalid_arg "Invalid argument of sixtyfour option: %s" v
    | _ -> usage ()
    | exception _ -> usage ()
  in
  let suffix = if is_64bit then "_64_backend.ml" else "_32_backend.ml" in
  print_string suffix
