module type Immediate = sig
  type t [@@immediate]
end

module type Boxed = sig
  type t
end

module Immediate64 (Immediate : Immediate) (Boxed : Boxed) = struct
  type t [@@immediate64]
  type 'a repr = Immediate : Immediate.t repr | Boxed : Boxed.t repr

  let repr =
    (* For soundness of the [@@immediate64] annotation, we ensure to use the
       boxed representation only when not on 64-bit platforms, but we need to
       use The Force to convince the type system of this fact. *)
    match Sys.word_size with
    | 64 -> (Obj.magic Immediate : t repr)
    | 32 -> (Obj.magic Boxed : t repr)
    | n -> Format.kasprintf failwith "Unknown word size: %d" n
end

module Optint = struct
  include Immediate64 (Optint_native) (Optint_emul)

  module type S = Integer_interface.S with type t := t

  let impl : (module S) =
    match repr with
    | Immediate -> (module Optint_native : S)
    | Boxed -> (module Optint_emul : S)

  include (val impl : S)
end

module Int63 = struct
  include Immediate64 (Int63_native) (Int63_emul)

  module type S = Integer_interface.S with type t := t

  let impl : (module S) =
    match repr with
    | Immediate -> (module Int63_native : S)
    | Boxed -> (module Int63_emul : S)

  include (val impl : S)
end

include Optint

module Private = struct
  module type S = Integer_interface.S

  module Int63_boxed = Int63_emul
end
