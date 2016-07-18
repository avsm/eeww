open Ocamlbuild_plugin

let _tag_name = "coverage"
let _environment_variable = "BISECT_COVERAGE"
let _enable = "YES"

let handle_coverage () =
  if getenv ~default:"" _environment_variable <> _enable then
    mark_tag_used _tag_name
  else begin
    flag ["ocaml"; "compile"; _tag_name] (S [A "-package"; A "bisect_ppx"]);
    flag ["ocaml"; "link"; _tag_name] (S [A "-package"; A "bisect_ppx"])
  end

let () =
  dispatch begin function
    | After_rules -> handle_coverage ()
    | _ -> ()
  end
