(** Misc. string functions not found in the built in OCaml string
    module *)

(** string_after [s] [n] returns the substring of [s] that is after
    character [n] *)
val string_after : string -> int -> string

(** equivalent to [Str.quote] *)
val quote : string -> string

(** split [?max] [s] [~on] splits [s] on every [on] occurence upto
    [max] number of items if [max] is specified. [max] is assumed to
    be a small number if specified. To not cause stack overflows *)
val split : ?max:int -> string -> on:char -> string list

(** split_delim_unbounded [s] [~on] will split [s] on every occurence
    of [on] but will add the separators between the tokens. Maintains
    the invariant:

    String.concat (split_delimc_unbounded s ~on) =s *)
val full_split : string -> on:char -> string list

(** Trims spaces on the left of the string *)
val trim_left : string -> string

(** split_strim_right [s] [~on] [~trim] splits [s] on every character
    in [on]. Characters in [trim] are trimmed from the left of every
    result element *)
val split_trim_left : string -> on:string -> trim:string -> string list
