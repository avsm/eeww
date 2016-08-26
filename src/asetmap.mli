(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Alternative standard library Sets and Maps

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Asetmap} *)

(** Sets *)
module Set : sig

  (** {1 Sets} *)

  (** Signature for sets of values of a totally ordered type. *)
  module type S = sig

    (** {1:set Set} *)

    include Set.S

    val find : elt -> t -> elt option
    (** [find e s] is the element of [s] equal to [e] (if any). *)

    val get : elt -> t -> elt
    (** [get] is like {!find} but @raise Invalid_argument if
        [elt] is not in [s]. *)

    val min_elt : t -> elt option
    (** [min_elt s] is the smallest element of [s] (if any). *)

    val get_min_elt : t -> elt
    (** [get_min_elt] is like {!min_elt} but @raise Invalid_argument
        on the empty set. *)

    val max_elt : t -> elt option
    (** [max_elt s] is the greatest element of [s] (if any). *)

    val get_max_elt : t -> elt
    (** [get_max_elt] is like {!max_elt} but @raise Invalid_argument
        on the empty set. *)

    val choose : t -> elt option
    (** [choose s] is an element of [s] or [None] is [s] empty. The
        chosen element is equal for equal sets. *)

    val get_any_elt : t -> elt
    (** [get_any_elt] is like {!choose} but @raise Invalid_argument on the
        empty set. *)

    (** {1:conv Conversions} *)

    val to_list : t -> elt list
    (** [to_list s] is the elements of [s] in increasing order. *)

    val of_list : elt list -> t
    (** [of_list l] is a set from the elements of [l] *)

    (** {1:pp Pretty-printers} *)

    val pp :
      ?sep:(Format.formatter -> unit -> unit) ->
      (Format.formatter -> elt -> unit) ->
      Format.formatter -> t -> unit
    (** [pp ~sep pp_elt ppf s] formats the elements of [s] on [ppf].
        Each element is formatted with [pp_elt] and elements are separated
        by [~sep] (defaults to {!Format.pp_print_cut}). If the set is empty
        leaves [ppf] untouched. *)

    val dump :
      (Format.formatter -> elt -> unit) ->
      Format.formatter -> t -> unit
    (** [dump pp_elt ppf s] prints an unspecified representation of [s] on
        [ppf] using [pp_elt] to print elements. *)
  end

  (** [Make (Ord)] is a set data structure for values of the totally
      ordered type [Ord]. *)
  module Make (Ord : Set.OrderedType) : S with type elt = Ord.t
end

(** Maps *)
module Map : sig

  (** {1 Maps} *)

  (** Signature for maps with keys of a totally ordered type. *)
  module type S = sig

    (** {1:map Map} *)

    include Map.S

    val find : key -> 'a t -> 'a option
    (** [find k m] is the binding of [k] in [m] (if any). *)

    val get : key -> 'a t -> 'a
    (** [get k m] is like {!Map.S.find} but raises [Invalid_argument] if
        [k] is not bound in [m]. *)

    val min_binding : 'a t -> (key * 'a) option
    (** [min_binding m] is the smallest binding of [m] (if any). *)

    val get_min_binding : 'a t -> (key * 'a)
    (** [get_min_binding] is like {!min_binding} but @raise Invalid_argument
        on the empty map. *)

    val max_binding : 'a t -> (key * 'a) option
    (** [max_binding m] is the greatest binding of [m] (if any). *)

    val get_max_binding : 'a t -> key * 'a
    (** [get_min_binding] is like {!max_binding} but @raise Invalid_argument
        on the empty map. *)

    val choose : 'a t -> (key * 'a) option
    (** [choose m] is a binding of [m] (if any). Equal bindings are chosen
        for equal maps. *)

    val get_any_binding : 'a t -> (key * 'a)
    (** [get_any_binding] is like {!choose} but @raise Invalid_argument
        on the empty map. *)

    (** {1:conv Conversions} *)

    val to_list : 'a t -> (key * 'a) list
    (** [to_list m] is the bindings of [m] in increasing key order. *)

    val of_list : (key * 'a) list -> 'a t
    (** [of_list l] is the map holding the bindings of [l]. If a key
        is bound more than once the last one takes over. *)

    (** {1:pp Pretty-printing} *)

    val pp : ?sep:(Format.formatter -> unit -> unit) ->
      (Format.formatter -> key * 'a -> unit) ->
      Format.formatter -> 'a t -> unit
    (** [pp ~sep pp_binding ppf m] formats the bindings of [m] on [ppf].
        Each binding is formatted with [pp_binding] and bindings are separated
        by [sep] (defaults to {!Format.pp_print_cut}). If the map is empty
        leaves [ppf] untouched. *)

    val dump :
      (Format.formatter -> key * 'a -> unit) ->
      Format.formatter -> 'a t -> unit
    (** [dump pp_binding] prints an unspecified represention of [m] on
        [ppf] using [pp_binding] to print the bindings. *)
  end

  (** Signature for maps with keys of a totally ordered type equiped
      with a set. *)
  module type S_with_key_set = sig
    include S
    type key_set
    val dom : 'a t -> key_set
    (** [dom m] is the domain of [m]. *)
  end

  module Make (Ord : Map.OrderedType) : S with type key = Ord.t
  (** [Make (Ord)] is a map data structure with keys of the totally
      ordered type [Ord]. *)

  module Make_with_key_set
      (Ord : Map.OrderedType)
      (Key_set : Set.S with type elt = Ord.t) : S_with_key_set
    with type key = Ord.t
     and type key_set = Key_set.t
  (** [Make_with_key_set (Ord) (Key_set)] is a map data structure
      with keys of the totally ordered type [Ord] and key sets [Key_set]. *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
