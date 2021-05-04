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

type 'a t = {
          dummy: 'a;
  mutable size:  int;
  mutable data:  'a array; (* 0 <= size <= Array.length data *)
}

let make n ~dummy =
  if n < 0 || n > Sys.max_array_length then invalid_arg "Vector.make";
  { dummy = dummy; size = n; data = Array.make n dummy; }

let create ~dummy =
  make 0 ~dummy

let init n ~dummy f =
  if n < 0 || n > Sys.max_array_length then invalid_arg "Vector.init";
  { dummy = dummy; size = n; data = Array.init n f; }

let length a =
  a.size

let get a i =
  if i < 0 || i >= a.size then invalid_arg "Vector.get";
  Array.unsafe_get a.data i

let set a i v =
  if i < 0 || i >= a.size then invalid_arg "Vector.set";
  Array.unsafe_set a.data i v

let unsafe_get a i =
  Array.unsafe_get a.data i

let unsafe_set a i v =
  Array.unsafe_set a.data i v

let resize a s =
  if s < 0 then invalid_arg "Vector.resize";
  let n = Array.length a.data in
  if s <= a.size then
    (* shrink *)
    if 4 * s < n then (* reallocate into a smaller array *)
      a.data <- Array.sub a.data 0 s
    else
      Array.fill a.data s (a.size - s) a.dummy
  else begin
    (* grow *)
    if s > n then begin (* reallocate into a larger array *)
      if s > Sys.max_array_length then invalid_arg "Vector.resize: cannot grow";
      let n' = min (max (2 * n) s) Sys.max_array_length in
      let a' = Array.make n' a.dummy in
      Array.blit a.data 0 a' 0 a.size;
      a.data <- a'
    end
  end;
  a.size <- s

(** stack interface *)

let is_empty a =
  length a = 0

let clear a =
  resize a 0

let push a v =
  let n = a.size in
  resize a (n+1);
  Array.unsafe_set a.data n v

exception Empty

let top a =
  let n = length a in
  if n = 0 then raise Empty;
  Array.unsafe_get a.data (n - 1)

let pop a =
  let n = length a - 1 in
  if n < 0 then raise Empty;
  let r = Array.unsafe_get a.data n in
  resize a n;
  r

(** array interface *)

let append a1 a2 =
  let n1 = length a1 in
  let n2 = length a2 in
  resize a1 (n1 + n2);
  for i = 0 to n2 - 1 do unsafe_set a1 (n1 + i) (unsafe_get a2 i) done

let copy a =
  { dummy = a.dummy;
    size = a.size;
    data = Array.copy a.data; }

let sub a ofs len =
  if len < 0 || ofs > length a - len then invalid_arg "Vector.sub";
  { dummy = a.dummy; size = len; data = Array.sub a.data ofs len }

let fill a ofs len v =
  if ofs < 0 || len < 0 || ofs > length a - len then invalid_arg "Vector.fill";
  for i = ofs to ofs + len - 1 do unsafe_set a i v done

let blit a1 ofs1 a2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length a1 - len
             || ofs2 < 0 || ofs2 > length a2 - len
  then invalid_arg "Vector.blit";
  if ofs1 <= ofs2 then
    for i = len - 1 downto 0 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done
  else
    for i = 0 to len - 1 do
      unsafe_set a2 (ofs2 + i) (unsafe_get a1 (ofs1 + i))
    done

let iter f a =
  for i = 0 to length a - 1 do f (unsafe_get a i) done

let map f a =
  { dummy = f a.dummy; size = a.size; data = Array.map f a.data }

let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let mapi f a =
  { dummy = f 0 a.dummy; size = a.size; data = Array.mapi f a.data }

let to_list a =
  let rec tolist i res =
    if i < 0 then res else tolist (i - 1) (unsafe_get a i :: res) in
  tolist (length a - 1) []

let of_list ~dummy l =
  let a = Array.of_list l in
  { dummy = dummy; size = Array.length a; data = a }

let to_array a =
  Array.sub a.data 0 a.size

let of_array ~dummy a =
  { dummy = dummy; size = Array.length a; data = Array.copy a }

let fold_left f x a =
  let r = ref x in
  for i = 0 to length a - 1 do r := f !r (unsafe_get a i) done;
  !r

let fold_right f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do r := f (unsafe_get a i) !r done;
  !r

