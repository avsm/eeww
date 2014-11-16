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

(** full_split [s] [~on] will split [s] on every occurence
    of [on] but will add the separators between the tokens. Maintains
    the invariant:

    String.concat (full_split s ~on) =s *)
val full_split : string -> on:char -> string list

(** Trims spaces on the left of the string *)
val trim_left : string -> string

(** split_strim_right [s] [~on] [~trim] splits [s] on every character
    in [on]. Characters in [trim] are trimmed from the left of every
    result element *)
val split_trim_left : string -> on:string -> trim:string -> string list

val of_list : char list -> string

val to_list : string -> char list

val to_array : string -> char array

val of_array : char array -> string

val find_from : ?start:int -> string -> pattern:string -> int option

val cut : string -> on:string -> (string * string) option
(** [String.cut on s] is either the pair [Some (l,r)] of the two
    (possibly empty) substrings of [s] that are delimited by the first
    match of the non empty onarator string [on] or [None] if [on]
    can't be matched in [s]. Matching starts from the beginning of [s].

    The invariant [l ^ on ^ r = s] holds. 

    @raise Invalid_argument if [on] is the empty string. *)

val rcut : string -> on:string -> (string * string) option
(** [String.rcut on s] is like {!cut} but the matching is done backwards
    starting from the end of [s].

    @raise Invalid_argument if [on] is the empty string. *)
