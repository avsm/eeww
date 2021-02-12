let unit_sexp_grammar : Raw_grammar.t = Inline (List [])
let bool_sexp_grammar : Raw_grammar.t = Inline (Atom Bool)
let string_sexp_grammar : Raw_grammar.t = Inline (Atom String)
let bytes_sexp_grammar : Raw_grammar.t = string_sexp_grammar
let char_sexp_grammar : Raw_grammar.t = Inline (Atom Char)
let int_sexp_grammar : Raw_grammar.t = Inline (Atom Int)
let float_sexp_grammar : Raw_grammar.t = Inline (Atom Float)
let int32_sexp_grammar : Raw_grammar.t = Inline (Atom Int)
let int64_sexp_grammar : Raw_grammar.t = Inline (Atom Int)
let nativeint_sexp_grammar : Raw_grammar.t = Inline (Atom Int)

let ref_sexp_grammar : Raw_grammar.t =
  Inline (Tyvar_parameterize ([ "'a" ], Tyvar_index 0))
;;

let lazy_t_sexp_grammar : Raw_grammar.t =
  Inline (Tyvar_parameterize ([ "'a" ], Tyvar_index 0))
;;

let option_sexp_grammar : Raw_grammar.t =
  Inline (Tyvar_parameterize ([ "'a" ], Option (Tyvar_index 0)))
;;

let list_sexp_grammar : Raw_grammar.t =
  Inline (Tyvar_parameterize ([ "'a" ], List [ Many (Tyvar_index 0) ]))
;;

let array_sexp_grammar : Raw_grammar.t = list_sexp_grammar
let empty_sexp_grammar : Raw_grammar.t = Inline (Union [])
let opaque_sexp_grammar : Raw_grammar.t = empty_sexp_grammar
let fun_sexp_grammar : Raw_grammar.t = empty_sexp_grammar

let tuple2_sexp_grammar : Raw_grammar.t =
  Inline
    (Tyvar_parameterize ([ "'a"; "'b" ], List [ One (Tyvar_index 0); One (Tyvar_index 1) ]))
;;
