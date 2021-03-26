module Optint : sig
  type t [@@immediate64]

  include Integer_interface.S with type t := t
  (** @inline *)
end

module Int63 : sig
  type t [@@immediate64]

  include Integer_interface.S with type t := t
  (** @inline *)
end

include module type of Optint
(** @inline *)
