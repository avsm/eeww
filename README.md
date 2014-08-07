stringext
=========

Extra string functions for OCaml. Mainly splitting. All functions are in the
`Stringext` module. Here's a snippet of most useful functions out of the mli:

```
(** split [?max] [s] [~on] splits [s] on every [on] occurence upto
    [max] number of items if [max] is specified. [max] is assumed to
    be a small number if specified. To not cause stack overflows *)
val split : ?max:int -> string -> on:char -> string list

(** full_split [s] [~on] will split [s] on every occurence
    of [on] but will add the separators between the tokens. Maintains
    the invariant:

    String.concat (full_split s ~on) =s *)
val full_split : string -> on:char -> string list
```
