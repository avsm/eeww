(** Extraction of [Stdlib.Sys.Immediate64] for pre-4.10 compatibility.

    For soundness of the [@@immediate64] annotation, we ensure to use the boxed
    representation only when not on 64-bit platforms, but we need to use The
    Force to convince the type system of this fact. *)
module Immediate64 = struct
  module type Non_immediate = sig
    type t
  end

  module type Immediate = sig
    type t [@@immediate]
  end

  module Make (Immediate : Immediate) (Non_immediate : Non_immediate) = struct
    type t [@@immediate64]

    type 'a repr =
      | Immediate : Immediate.t repr
      | Non_immediate : Non_immediate.t repr

    external magic : _ repr -> t repr = "%identity"

    let repr =
      if Sys.word_size = 64 then magic Immediate else magic Non_immediate
  end
end

module Optint = struct
  include Immediate64.Make (Optint_native) (Optint_emul)

  module type S = Integer_interface.S with type t := t

  let impl : (module S) =
    match repr with
    | Immediate -> (module Optint_native : S)
    | Non_immediate -> (module Optint_emul : S)

  include (val impl : S)
end

module Int63 = struct
  include Immediate64.Make (Int63_native) (Int63_emul)

  module type S = Integer_interface.S with type t := t

  let impl : (module S) =
    match repr with
    | Immediate -> (module Int63_native : S)
    | Non_immediate -> (module Int63_emul : S)

  include (val impl : S)
end

include Optint

module Private = struct
  module type S = Integer_interface.S

  module Int63_boxed = Int63_emul
end
