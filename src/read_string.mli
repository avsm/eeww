module BE : sig
  (** Functions reading according to Big Endian byte order *)

  val get_char : string -> int -> char
  (** [get_char buff i] reads 1 byte at offset i as a char *)

  val get_uint8 : string -> int -> int
  (** [get_uint8 buff i] reads 1 byte at offset i as an unsigned int of 8
  bits. i.e. It returns a value between 0 and 2^8-1 *)

  val get_int8 : string -> int -> int
  (** [get_int8 buff i] reads 1 byte at offset i as a signed int of 8
  bits. i.e. It returns a value between -2^7 and 2^7-1 *)

  val get_uint16 : string -> int -> int
  (** [get_uint16 buff i] reads 2 bytes at offset i as an unsigned int
  of 16 bits. i.e. It returns a value between 0 and 2^16-1 *)

  val get_int16 : string -> int -> int
  (** [get_int16 buff i] reads 2 byte at offset i as a signed int of
  16 bits. i.e. It returns a value between -2^15 and 2^15-1 *)

  val get_int32 : string -> int -> int32
  (** [get_int32 buff i] reads 4 bytes at offset i as an int32. *)

  val get_int64 : string -> int -> int64
  (** [get_int64 buff i] reads 8 bytes at offset i as an int64. *)

  val set_char : string -> int -> char -> unit
  (** [set_char buff i v] writes [v] to [buff] at offset [i] *)

  val set_int8 : string -> int -> int -> unit
  (** [set_int8 buff i v] writes the least significant 8 bits of [v]
  to [buff] at offset [i] *)

  val set_int16 : string -> int -> int -> unit
  (** [set_int16 buff i v] writes the least significant 16 bits of [v]
  to [buff] at offset [i] *)

  val set_int32 : string -> int -> int32 -> unit
  (** [set_int32 buff i v] writes [v] to [buff] at offset [i] *)

  val set_int64 : string -> int -> int64 -> unit
  (** [set_int64 buff i v] writes [v] to [buff] at offset [i] *)

end

module BE_unsafe : sig
  (** Functions reading according to Big Endian byte order without
  checking for overflow *)

  include module type of BE

end

module LE : sig
  (** Functions reading according to Little Endian byte order *)

  include module type of BE

end

module LE_unsafe : sig
  (** Functions reading according to Big Endian byte order without
  checking for overflow *)

  include module type of BE

end
