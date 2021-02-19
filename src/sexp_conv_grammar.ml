let unit_sexp_grammar : unit Raw_grammar.t = { untyped = List Empty }
let bool_sexp_grammar : bool Raw_grammar.t = { untyped = Bool }
let string_sexp_grammar : string Raw_grammar.t = { untyped = String }
let bytes_sexp_grammar : bytes Raw_grammar.t = { untyped = String }
let char_sexp_grammar : char Raw_grammar.t = { untyped = Char }
let int_sexp_grammar : int Raw_grammar.t = { untyped = Integer }
let float_sexp_grammar : float Raw_grammar.t = { untyped = Float }
let int32_sexp_grammar : int32 Raw_grammar.t = { untyped = Integer }
let int64_sexp_grammar : int64 Raw_grammar.t = { untyped = Integer }
let nativeint_sexp_grammar : nativeint Raw_grammar.t = { untyped = Integer }
let ref_sexp_grammar grammar = Raw_grammar.coerce grammar
let lazy_t_sexp_grammar grammar = Raw_grammar.coerce grammar

let option_sexp_grammar ({ untyped } : _ Raw_grammar.t) : _ option Raw_grammar.t =
  { untyped = Option untyped }
;;

let list_sexp_grammar ({ untyped } : _ Raw_grammar.t) : _ list Raw_grammar.t =
  { untyped = List (Many untyped) }
;;

let array_sexp_grammar ({ untyped } : _ Raw_grammar.t) : _ array Raw_grammar.t =
  { untyped = List (Many untyped) }
;;

let empty_sexp_grammar : _ Raw_grammar.t = { untyped = Union [] }
let opaque_sexp_grammar = empty_sexp_grammar
let fun_sexp_grammar = empty_sexp_grammar
