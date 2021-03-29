type t [@@immediate64]
(** The type of integers with {i at least} 32 bits.
    For 63-bit integers, see {!Int63}. *)

include Integer_interface.S with type t := t
(** @inline *)

(** {1 Other modules} *)

module Int63 : sig
  type t [@@immediate64]
  (** The type of integers with exactly 63-bits. *)

  include Integer_interface.S with type t := t
  (** @inline *)
end

(** Utilities with no stability guarantee, exposed for internal use. *)
module Private : sig
  module type S = Integer_interface.S

  module Int63_boxed = Int63_emul
  (** An implementation of 63-bit integers that always uses a boxed
      representation regardless of word size. *)
end
