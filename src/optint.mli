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

(** Utilities with no stability guarantee, exposed for internal use. *)
module Private : sig
  module type S = Integer_interface.S

  module Int63_boxed = Int63_emul
  (** An implementation of 63-bit integers that always uses a boxed
      representation regardless of word size. *)
end
