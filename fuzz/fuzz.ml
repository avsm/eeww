let () =
  Crowbar.add_test ~name:"identity with int32" Crowbar.[ int32 ] @@ fun i32 ->
  let v = Optint.of_int32 i32 in
  let u = Optint.to_int32 v in
  Crowbar.check_eq ~pp:Fmt.int32 ~eq:Int32.equal ~cmp:Int32.compare i32 u

let () =
  Crowbar.add_test ~name:"identity with int" Crowbar.[ int ] @@ fun i ->
  let v = Optint.of_int i in
  let u = Optint.to_int v in
  Crowbar.check_eq ~pp:Fmt.int ~eq:(=) ~cmp:compare i u

let binary_operator =
  Crowbar.(choose [ const `Add
                  ; const `Sub
                  ; const `Mul
                  ; const `Div
                  ; const `Rem
                  ; const `Lor
                  ; const `Land
                  ; const `Lxor ])
let unary_operator =
  Crowbar.(choose [ const `Neg
                  ; const `Succ
                  ; const `Pred
                  ; const `Lnot ])

type binary = [ `Add | `Sub | `Mul | `Div | `Rem | `Lor | `Land | `Lxor ]
type unary = [ `Neg | `Succ | `Pred | `Lnot ]

let min_intl = Int32.to_int Int32.min_int
let max_intl = Int32.to_int Int32.max_int

let generate ~of_int =
  let edge = Crowbar.map [ Crowbar.range ~min:min_intl max_intl ] @@ fun v -> `V (of_int v) in

  let edge_binary = Crowbar.map [ edge; edge; binary_operator ] @@ fun a b o -> [ a; b; o ] in
  let node_binary = Crowbar.map [ edge; binary_operator ] @@ fun x o -> [ x; o ] in

  let edge_unary = Crowbar.map [ edge; unary_operator ] @@ fun x o -> [ x; o ] in
  let node_unary = Crowbar.map [ unary_operator ] @@ fun o -> [ o ] in

  let edge = Crowbar.map [ edge ] @@ fun x -> [ x ] in
  let edge = Crowbar.choose [ edge; edge_binary; edge_unary ] in
  let node = Crowbar.choose [ node_binary; node_unary ] in

  Crowbar.(map [ edge; list node ] @@ fun x r -> List.concat (x :: r))

module type ARITHMETIC = sig
  type t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val logor : t -> t -> t
  val logand : t -> t -> t
  val logxor : t -> t -> t

  val abs : t -> t
  val neg : t -> t
  val succ : t -> t
  val pred : t -> t
  val lognot : t -> t
end

type 'v p = [ binary | unary | `V of 'v ]

let pp_p ~pp_v ppf = function
  | `Add -> Fmt.string ppf "+" | `Sub -> Fmt.string ppf "-" | `Mul -> Fmt.string ppf "*" | `Div -> Fmt.string ppf "/" | `Rem -> Fmt.string ppf "%"
  | `Lor -> Fmt.string ppf "|" | `Land -> Fmt.string ppf "&" | `Lxor -> Fmt.string ppf "^"
  | `Neg -> Fmt.string ppf "neg" | `Succ -> Fmt.string ppf "succ" | `Pred -> Fmt.string ppf "pred"
  | `Lnot -> Fmt.string ppf "~"
  | `V v -> pp_v ppf v

let rec binary
  : type v. (module ARITHMETIC with type t = v) -> v -> v -> binary -> v p list -> v
  = fun (module Arith) a b o r ->
    let open Arith in

    match o with
    | `Add -> eval (module Arith) (`V (add a b) :: r)
    | `Sub -> eval (module Arith) (`V (sub a b) :: r)
    | `Mul -> eval (module Arith) (`V (mul a b) :: r)
    | `Div -> eval (module Arith) (`V (div a b) :: r)
    | `Rem -> eval (module Arith) (`V (rem a b) :: r)
    | `Lor -> eval (module Arith) (`V (logor a b) :: r)
    | `Land -> eval (module Arith) (`V (logand a b) :: r)
    | `Lxor -> eval (module Arith) (`V (logxor a b) :: r)

and unary
  : type v. (module ARITHMETIC with type t = v) -> v -> unary -> v p list -> v
  = fun (module Arith) x o r ->
    let open Arith in

    match o with
    | `Neg -> eval (module Arith) (`V (neg x) :: r)
    | `Succ -> eval (module Arith) (`V (succ x) :: r)
    | `Pred -> eval (module Arith) (`V (pred x) :: r)
    | `Lnot -> eval (module Arith) (`V (lognot x) :: r)

and eval
  : type v. (module ARITHMETIC with type t = v) -> v p list -> v
  = fun arith -> function
    | (`V a) :: (`V b) :: (#binary as o) :: r -> binary arith a b o r
    | (`V x) :: (#unary as o) :: r -> unary arith x o r
    | [ `V v ] -> v
    | _ -> Crowbar.bad_test ()

let () =
  Crowbar.add_test ~name:"computation" Crowbar.[ generate ~of_int:(fun x -> x) ] @@ fun l ->

  let la = List.map
      (function `V x -> `V (Optint.of_int x)
        | (#binary | #unary) as x -> (x :> Optint.t p)) l in
  let lb = List.map
      (function `V x -> `V (Int32.of_int x)
        | (#binary | #unary) as x -> (x :> int32 p)) l in

  let a = eval (module Optint) la in
  let b = eval (module Int32) lb in
  let a = Optint.to_int a in
  let b = Int32.to_int b in

  Crowbar.check_eq ~pp:Fmt.int ~eq:(=) ~cmp:compare (a land 0xffffffff) (b land 0xffffffff)
