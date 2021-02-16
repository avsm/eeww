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
  let is_x64 =
    let usage () = invalid_arg "%s --x64 (true|false) -o <output>" Sys.argv.(0) in
    match Sys.argv with
    | [|_; "--x64"; ("true" | "!true")|] -> true
    | [|_; "--x64"; ("false" | "!false")|] -> false
    | [|_; "--x64"; v |] -> invalid_arg "Invalid argument of x64 option: %s" v
    | _ -> usage ()
    | exception _ -> usage ()
  in
  let suffix = if is_x64 then "_x64_backend.ml" else "_x86_backend.ml" in
  print_string suffix
