let invalid_arg fmt = Format.ksprintf (fun s -> invalid_arg s) fmt

let () =
  let is_x64, output =
    try match Sys.argv with
        | [| _; "--x64"; is_x64; "-o"; output; |] ->
           let is_x64 = match is_x64 with
             | "true" | "!true" -> true
             | "false" | "!false" -> false
             | v -> invalid_arg "Invalid argument of x64 option: %s" v in
           is_x64, output
        | _ -> invalid_arg "%s --x64 (true|false) -o <output>" Sys.argv.(0)
    with _ -> invalid_arg "%s --x64 (true|false) -o <output>" Sys.argv.(0) in
  let oc = open_out output in
  let backend =
    if is_x64 then "Int_x64_backend" else "Int_x86_backend" in
  Printf.fprintf oc "include %s\n%!" backend;
  close_out oc
