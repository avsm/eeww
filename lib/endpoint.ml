module Type = struct
  type t = Invalid | Address | Host | Bonjour | Url
end

type t = unit
(** The type for endpoints *)

let get_type _ = Type.Invalid
