#use "topfind";;
#require "base";;
#load "ppxlib_metaquot_lifters.cmo";;
#load "ppxlib_metaquot.cmo";;

open Ppxlib


let foo =
  Deriving.add "foo"
    ~str_type_decl:(Deriving.Generator.make_noarg
                      (fun ~loc ~path:_ _ -> [%str let x = 42]))
[%%expect{|
val foo : Deriving.t = <abstr>
|}]

let bar =
  Deriving.add "bar"
    ~str_type_decl:(Deriving.Generator.make_noarg
                      ~deps:[foo]
                      (fun ~loc ~path:_ _ -> [%str let () = Printf.printf "x = %d\n" x]))
[%%expect{|
val bar : Deriving.t = <abstr>
|}]

let mtd =
  Deriving.add "mtd"
    ~sig_module_type_decl:(
      Deriving.Generator.make_noarg
        (fun ~loc ~path:_ _ -> [%sig: val y : int]))
    ~str_module_type_decl:(
      Deriving.Generator.make_noarg
        (fun ~loc ~path:_ _ -> [%str let y = 42]))
[%%expect{|
val mtd : Deriving.t = <abstr>
|}]

type t = int [@@deriving bar]
[%%expect{|
Line _, characters 25-28:
Error: Deriver foo is needed for bar, you need to add it before in the list
|}]

type t = int [@@deriving bar, foo]
[%%expect{|
Line _, characters 25-33:
Error: Deriver foo is needed for bar, you need to add it before in the list
|}]

type nonrec int = int [@@deriving foo, bar]
[%%expect{|
type nonrec int = int
val x : int = 42
|}]

module type X = sig end [@@deriving mtd]
[%%expect{|
module type X = sig  end
val y : int = 42
|}]

module Y : sig
  module type X = sig end [@@deriving mtd]
end = struct
  module type X = sig end
  let y = 42
end
[%%expect{|
Line _, characters 42-42:
Error: ppxlib: [@@@deriving.end] attribute missing
|}]
