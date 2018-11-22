let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let load_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ; close_in ic ; Bytes.unsafe_to_string s

let store_file path contents =
  let oc = open_out path in
  output_string oc contents ; close_out oc

let () =
  let is_x64, output =
    try
      match Sys.argv with
      | [|_; "--x64"; is_x64; "-o"; output|] ->
          let is_x64 =
            match is_x64 with
            | "true" | "!true" -> true
            | "false" | "!false" -> false
            | v -> invalid_arg "Invalid argument of x64 option: %s" v
          in
          (is_x64, output)
      | _ -> invalid_arg "%s --x64 (true|false) -o <output>" Sys.argv.(0)
    with _ -> invalid_arg "%s --x64 (true|false) -o <output>" Sys.argv.(0)
  in
  let contents_ml =
    load_file (if is_x64 then "int_x64_backend.ml" else "int_x86_backend.ml")
  in
  let contents_mli =
    load_file (if is_x64 then "int_x64_backend.mli" else "int_x86_backend.mli")
  in
  store_file (output ^ ".ml") contents_ml ;
  store_file (output ^ ".mli") contents_mli
