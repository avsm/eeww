(** Sexp grammar definitions. *)

val unit_sexp_grammar : unit Raw_grammar.t
val bool_sexp_grammar : bool Raw_grammar.t
val string_sexp_grammar : string Raw_grammar.t
val bytes_sexp_grammar : bytes Raw_grammar.t
val char_sexp_grammar : char Raw_grammar.t
val int_sexp_grammar : int Raw_grammar.t
val float_sexp_grammar : float Raw_grammar.t
val int32_sexp_grammar : int32 Raw_grammar.t
val int64_sexp_grammar : int64 Raw_grammar.t
val nativeint_sexp_grammar : nativeint Raw_grammar.t
val ref_sexp_grammar : 'a Raw_grammar.t -> 'a ref Raw_grammar.t
val lazy_t_sexp_grammar : 'a Raw_grammar.t -> 'a lazy_t Raw_grammar.t
val option_sexp_grammar : 'a Raw_grammar.t -> 'a option Raw_grammar.t
val list_sexp_grammar : 'a Raw_grammar.t -> 'a list Raw_grammar.t
val array_sexp_grammar : 'a Raw_grammar.t -> 'a array Raw_grammar.t
val opaque_sexp_grammar : 'a Raw_grammar.t
val fun_sexp_grammar : 'a Raw_grammar.t
