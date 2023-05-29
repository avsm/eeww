(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module Set = struct
  module type S = sig
    include Set.S

    val find : elt -> t -> elt option
    val get : elt -> t -> elt

    val min_elt : t -> elt option
    val get_min_elt : t -> elt

    val max_elt : t -> elt option
    val get_max_elt : t -> elt

    val choose : t -> elt option
    val get_any_elt : t -> elt

    val to_list : t -> elt list
    val of_list : elt list -> t

    val pp :
      ?sep:(Format.formatter -> unit -> unit) ->
      (Format.formatter -> elt -> unit) ->
      Format.formatter -> t -> unit

    val dump :
      (Format.formatter -> elt -> unit) ->
      Format.formatter -> t -> unit
  end

  module Make (Ord : Set.OrderedType) = struct
    include Set.Make (Ord)

    let get k s = try find k s with Not_found -> invalid_arg "elt not in set"
    let find k s = try Some (find k s) with Not_found -> None

    let err_empty () = invalid_arg "empty set"

    let get_min_elt s = try min_elt s with Not_found -> err_empty ()
    let min_elt s = try Some (min_elt s) with Not_found -> None

    let get_max_elt s = try max_elt s with Not_found -> err_empty ()
    let max_elt s = try Some (max_elt s) with Not_found -> None

    let get_any_elt s = try choose s with Not_found -> err_empty ()
    let choose s = try Some (choose s) with Not_found -> None

    let of_list l = List.fold_left (fun acc elt -> add elt acc) empty l
    let to_list s = List.rev (fold (fun elt acc -> elt :: acc) s [])

    let pp ?sep:(pp_sep = Format.pp_print_cut) pp_elt ppf s =
      let pp_elt elt is_first =
        if is_first then () else pp_sep ppf ();
        pp_elt ppf elt; false
      in
      ignore (fold pp_elt s true)

    let dump pp_elt ppf s =
      let pp_elt elt is_first =
        if is_first then () else Format.fprintf ppf "@ ";
        pp_elt ppf elt; false
      in
      Format.fprintf ppf "@[<1>{";
      ignore (fold pp_elt s true);
      Format.fprintf ppf "}@]";
      ()
  end
end

module Map = struct
  module type S = sig
    include Map.S

    val find : key -> 'a t -> 'a option
    val get : key -> 'a t -> 'a

    val min_binding : 'a t -> (key * 'a) option
    val get_min_binding : 'a t -> (key * 'a)

    val max_binding : 'a t -> (key * 'a) option
    val get_max_binding : 'a t -> (key * 'a)

    val choose : 'a t -> (key * 'a) option
    val get_any_binding : 'a t -> (key * 'a)

    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t

    val pp : ?sep:(Format.formatter -> unit -> unit) ->
      (Format.formatter -> key * 'a -> unit) ->
      Format.formatter -> 'a t -> unit

    val dump :
      (Format.formatter -> key * 'a -> unit) ->
      Format.formatter -> 'a t -> unit
  end

  module type S_with_key_set = sig
    include S
    type key_set
    val dom : 'a t -> key_set
  end

  module Make (Ord : Map.OrderedType) = struct
    include Map.Make (Ord)

    let get k s = try find k s with Not_found -> invalid_arg "key unbound"
    let find k m = try Some (find k m) with Not_found -> None

    let err_empty () = invalid_arg "empty map"

    let get_min_binding m = try min_binding m with Not_found -> err_empty ()
    let min_binding m = try Some (min_binding m) with Not_found -> None

    let get_max_binding m = try max_binding m with Not_found -> err_empty ()
    let max_binding m = try Some (max_binding m) with Not_found -> None

    let get_any_binding m = try choose m with Not_found -> err_empty ()
    let choose m = try Some (choose m) with Not_found -> None

    let to_list = bindings
    let of_list bs = List.fold_left (fun m (k,v) -> add k v m) empty bs

    let pp ?sep:(pp_sep = Format.pp_print_cut) pp_binding ppf m =
      let pp_binding k v is_first =
        if is_first then () else pp_sep ppf ();
        pp_binding ppf (k, v); false
      in
      ignore (fold pp_binding m true)

    let dump pp_binding ppf m =
      let pp_binding k v is_first =
        if is_first then () else Format.fprintf ppf "@ ";
        pp_binding ppf (k, v); false
      in
      Format.fprintf ppf "@[<1>{";
      ignore (fold pp_binding m true);
      Format.fprintf ppf "}@]";
      ()
  end

  module Make_with_key_set
      (Ord : Map.OrderedType)
      (Key_set : Set.S with type elt = Ord.t) = struct

    include Make (Ord)

    type key_set = Key_set.t
    let dom m = fold (fun k _ acc -> Key_set.add k acc) m Key_set.empty
  end
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
