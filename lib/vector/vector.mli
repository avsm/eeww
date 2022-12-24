(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Vectors (aka resizable arrays, growing arrays, dynamic arrays, etc.)

    This module implements arrays that automatically expand as necessary.
    Its implementation uses a traditional array and replaces it with a
    larger array when needed (and elements are copied from the old array
    to the new one). The current implementation doubles the capacity when
    growing the array (and shrinks it whenever the number of elements
    comes to one fourth of the capacity).

    The unused part of the internal array is filled with a dummy value,
    which is user-provided at creation time (and referred to below
    as ``the dummy value''). Consequently, vectors do not retain pointers
    to values that are not used anymore after a shrinking.

    Vectors provide an efficient implementation of stacks, with a
    better locality of reference than list-based implementations (such
    as standard library {!Stack}). A stack interface is provided,
    similar to that of {!Stack} (though {!Vector.push} have arguments
    in the other way round). Inserting [n] elements with
    {!Vector.push} has overall complexity O(n) i.e. each insertion has
    amortized constant time complexity. *)

type 'a t
  (** The polymorphic type of vectors.
      This is a mutable data type. *)


(** {2 Operations proper to vectors, or with a different type and/or
    semantics than those of module [Array]} *)

val make: int -> dummy:'a -> 'a t
(** [Vector.make n dummy] returns a fresh vector of length [n].
   All the elements of this new vector are initially
   physically equal to [dummy] (in the sense of the [==] predicate).

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
   If the value of [dummy] is a floating-point number, then the maximum
   size is only [Sys.max_array_length / 2].*)

val create: dummy:'a -> 'a t
(** [Vector.create dummy] returns a fresh vector of length [0]. *)

val init: int -> dummy:'a -> (int -> 'a) -> 'a t
(** [Vector.init n f] returns a fresh vector of length [n],
   with element number [i] initialized to the result of [f i].
   In other terms, [Vector.init n f] tabulates the results of [f]
   applied to the integers [0] to [n-1].

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
   If the return type of [f] is [float], then the maximum
   size is only [Sys.max_array_length / 2].*)

val resize: 'a t -> int -> unit
(** [Vector.resize a n] sets the length of vector [a] to [n].

   The elements that are no longer part of the vector, if any, are
   internally replaced by the dummy value of vector [a], so that they
   can be garbage collected when possible.

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length]. *)


(** {2 Stack interface}

    Contrary to standard library's {Stack}, module {Vector} uses less space
    (between N and 2N words, instead of 3N) and has better data locality. *)

val push: 'a t -> 'a -> unit
(** [Vector.push a x] appends [x] at the end of vector [a], i.e.,
    increases the size of [a] by one and stores [x] at the rightmost
    position.

    Note: the order of the arguments is not that of {!Stack.push}. *)

exception Empty
(** Raised when {!Vector.pop} or {!Vector.top} is applied to an empty vector. *)

val pop: 'a t -> 'a
(** [Vector.pop a] removes and returns the rightmost element in vector [a],
   or raises [Empty] if the stack is empty. *)

val top: 'a t -> 'a
(** [Vector.top a] returns the rightmost element in vector [a],
   or raises [Empty] if the vector is empty. *)

val clear: 'a t -> unit
(** Discard all elements from a vector.
    This is equivalent to setting the size to 0 with [resize]. *)

val is_empty: 'a t -> bool
(** Return [true] if the given vector is empty, [false] otherwise. *)


(** {2 Array interface} *)

val length: 'a t -> int
(** Return the length (number of elements) of the given vector.
    Note: the number of memory words occupiedby the vector can be larger. *)

val get: 'a t -> int -> 'a
(** [Vector.get a n] returns the element number [n] of vector [a].
    The first element has number [0]. The last element has number
    [Vector.length a - 1].

    Raise [Invalid_argument "Vector.get"]
    if [n] is outside the range [0] to [Vector.length a - 1]. *)

val set: 'a t -> int -> 'a -> unit
(** [Vector.set a n x] modifies vector [a] in place, replacing
   element number [n] with [x].

   Raise [Invalid_argument "Vector.set"]
   if [n] is outside the range 0 to [Vector.length a - 1]. *)

val append: 'a t -> 'a t -> unit
(** [Vector.append a1 a2] appends the elements of vector [a2] to the end
    of vector [a1].
    It works correctly even if [a1] and [a2] are the same vector. *)

val sub: 'a t -> int -> int -> 'a t
(** [Vector.sub a start len] returns a fresh vector of length [len], containing
    the elements number [start] to [start + len - 1] of vector [a]. *)

val copy: 'a t -> 'a t
(** [Vector.copy a] returns a copy of [a], that is, a fresh vector containing
    the same elements as [a]. *)

val fill : 'a t -> int -> int -> 'a -> unit
(** [Vector.fill a ofs len x] modifies the vector [a] in place,
    storing [x] in elements number [ofs] to [ofs + len - 1].

    Raise [Invalid_argument "Vector.fill"] if [ofs] and [len] do not
    designate a valid subvector of [a]. *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [Vector.blit v1 o1 v2 o2 len] copies [len] elements
   from vector [v1], starting at element number [o1], to vector [v2],
   starting at element number [o2]. It works correctly even if
   [v1] and [v2] are the same vector, and the source and
   destination chunks overlap.

   Raise [Invalid_argument "Vector.blit"] if [o1] and [len] do not
   designate a valid subvector of [v1], or if [o2] and [len] do not
   designate a valid subvector of [v2]. *)

val swap_contents : 'a t -> 'a t -> unit
(** [Vector.swap_contents v1 v2] exchanges the contents of vectors [v1]
    and [v2] in constant time. *)

val to_list : 'a t -> 'a list
(** [Vector.to_list a] returns the list of all the elements of [a]. *)

val of_list: dummy:'a -> 'a list -> 'a t
(** [Vector.of_list dummy l] returns a fresh vector containing the elements
    of [l]. *)

val to_array: 'a t -> 'a array
(** [Vector.to_array a] returns the array of all the elements of [a]. *)

val of_array: dummy:'a -> 'a array -> 'a t
(** [Vector.of_array dummy a] returns a fresh vector containing the elements
    of [a]. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [Vector.iter f a] applies function [f] in turn to all
   the elements of [a].  It is equivalent to
   [f (get a 0); f (get a 1); ...; f (get a (Vector.length a - 1))]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [Vector.map f a] applies function [f] to all the elements of [a],
   and builds a fresh vector with the results returned by [f].

   Note: the dummy value of the returned vector is obtained by applying
   [f] to the dummy value of [a]. If this is not what you want,
   first create a new vector and then fill it with the value
   [f (get a 0)], [f (get a 1)], etc. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Same as {!Vector.iter}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Same as {!Vector.map}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument.

   Note: the dummy value of the returned vector is obtained by applying
   [f 0] to the dummy value of [a].  *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [Vector.fold_left f x a] computes
   [f (... (f (f x (get a 0)) (get a 1)) ...) (get a (n-1))],
   where [n] is the length of the vector [a]. *)

val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a
(** [Vector.fold_right f a x] computes
   [f (get a 0) (f (get a 1) ( ... (f (get a (n-1)) x) ...))],
   where [n] is the length of the vector [a]. *)


(** {2 Only if you know what you are doing...} *)

val unsafe_get : 'a t -> int -> 'a
val unsafe_set : 'a t -> int -> 'a -> unit

