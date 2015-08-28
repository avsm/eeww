open QCheck

let (|>) x f = f x

let quoted_str = Printf.sprintf "%S"
let _ = run (
  let f1 s = Stringext.split_trim_left ~on:"," ~trim:" " s in
  let f2 s = Stringext.split ~on:',' s |> List.map Stringext.trim_left in
  let pp str =
    (quoted_str str) ^ " -> " ^
    (PP.list quoted_str (f1 str)) ^ " != " ^
    (PP.list quoted_str (f2 str))
  in
  mk_test ~name:"stringext.split_trim_left == split |> trim_left" ~pp ~size:String.length
    Arbitrary.(lift2 String.concat (among [",";" ,";", ";" , "]) (list string))
    Prop.((fun s -> f1 s = f2 s))
)
