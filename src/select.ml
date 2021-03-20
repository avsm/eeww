let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let write_to_file ~path str =
  let oc = open_out path in
  output_string oc str;
  close_out oc

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
  let suffix, type_ =
    match is_64bit with
    | true -> "_64_backend.ml", "type t [@@immediate64]"
    | false -> "_32_backend.ml", "type t"
  in
  write_to_file ~path:"selected_suffix" suffix;
  write_to_file ~path:"selected_type" type_
