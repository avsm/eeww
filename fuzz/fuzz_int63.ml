open Monolith

let int = le Int.max_int

let int32 =
  let gen_random =
    Random.self_init ();
    let open Int32 in
    let bits () = of_int (Random.bits ()) in
    fun () -> logxor (bits ()) (shift_left (bits ()) 30)
  in
  let pos = easily_constructible gen_random PPrintOCaml.int32 in
  let neg = deconstructible PPrintOCaml.int32 in
  ifpol pos neg

let float = deconstructible PPrintOCaml.float
let string = deconstructible PPrint.string

module type INTEGER = module type of Int63

module Fuzz_integer_equivalence (Reference : INTEGER) (Candidate : INTEGER) =
struct
  module R = Reference
  module C = Candidate

  let run t fuel =
    let endo = t ^> t in
    let binop = t ^> t ^> t in
    let binop_exn = t ^> t ^!> t in

    declare "zero" t R.zero C.zero;
    declare "one" t R.one C.one;
    declare "minus_one" t R.minus_one C.minus_one;
    declare "max_int" t R.max_int C.max_int;
    declare "min_int" t R.min_int C.min_int;

    declare "succ" endo R.succ C.succ;
    declare "pred" endo R.pred C.pred;
    declare "abs" endo R.abs C.abs;
    declare "neg" endo R.neg C.neg;
    declare "add" binop R.add C.add;
    declare "sub" binop R.sub C.sub;
    declare "mul" binop R.mul C.mul;
    declare "div" binop_exn R.div C.div;
    declare "rem" binop_exn R.rem C.rem;
    declare "logand" binop R.logand C.logand;
    declare "logor" binop R.logor C.logor;
    declare "logxor" binop R.logxor C.logxor;
    declare "lognot" endo R.lognot C.lognot;
    declare "shift_left" (t ^> int ^> t) R.shift_left C.shift_left;
    declare "shift_right" (t ^> int ^> t) R.shift_right C.shift_right;
    declare "shift_right_logical"
      (t ^> int ^> t)
      R.shift_right_logical C.shift_right_logical;

    declare "compare" (t ^> t ^> int) R.compare C.compare;
    declare "equal" (t ^> t ^> bool) R.equal C.equal;

    declare "of_int" (int ^> t) R.of_int C.of_int;
    declare "to_int" (t ^> int) R.to_int C.to_int;
    declare "of_int32" (int32 ^> t) R.of_int32 C.of_int32;
    declare "to_int32" (t ^> int32) R.to_int32 C.to_int32;
    declare "to_float" (t ^> float) R.to_float C.to_float;
    declare "to_string" (t ^> string) R.to_string C.to_string;

    main fuel
end

module Int63_equiv = Fuzz_integer_equivalence (Int63) (Int63_boxed)

let () =
  let t : (Int63.t, Int63_boxed.t) spec = declare_abstract_type () in
  Int63_equiv.run t 5
