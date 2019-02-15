(* (c) 2017 Hannes Mehnert, all rights reserved *)

open Astring

type s = string array

let root = Array.make 0 ""

let [@inline always] check_host_label s =
  String.get s 0 <> '-' && (* leading may not be '-' *)
  String.for_all (function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> true
      | _ -> false)
    s (* only LDH (letters, digits, hyphen)! *)

let is_hostname t =
  (* TLD should not be all-numeric! *)
  (if Array.length t > 0 then
     String.exists Char.Ascii.is_letter (Array.get t 0)
   else true) &&
  Array.for_all check_host_label t

let check_service_label s =
  match String.cut ~sep:"_" s with
  | None -> false
  | Some (empty, srv) ->
    if String.length empty > 0 then
      false
    else
      let slen = String.length srv in
      if slen > 0 && slen <= 15 then
        (* service must be LDH,
           hyphen _not_ at begin nor end, no hyphen following a hyphen
           1-15 characters *)
        let v, _ = String.fold_left (fun (valid, h) c ->
            let h' = c = '-' in
            let v = Char.Ascii.(is_letter c || is_digit c) || h' in
            let hh = not (h && h') in
            (v && valid && hh, h'))
            (true, false) srv
        in
        v && String.get srv 0 <> '-' && String.get srv (pred slen) <> '-'
      else
        false

let [@inline always] is_proto s =
  s = "_tcp" || s = "_udp" || s = "_sctp"

let [@inline always] check_label_length s =
  let l = String.length s in
  l < 64 && l > 0

let [@inline always] check_total_length t =
  Array.fold_left (fun acc s -> acc + 1 + String.length s) 1 t <= 255

let is_service t =
  let l = Array.length t in
  if l > 2 then
    let name = Array.sub t 0 (l - 2) in
    check_service_label (Array.get t (l - 1)) &&
    is_proto (Array.get t (l - 2)) &&
    Array.for_all check_label_length name &&
    check_total_length t &&
    is_hostname name
  else
    false

let [@inline always] check hostname t =
  Array.for_all check_label_length t &&
  check_total_length t &&
  if hostname then is_hostname t else true

let prepend_exn ?(hostname = true) xs lbl =
  let n = Array.make 1 lbl in
  let n = Array.append xs n in
  if check hostname n then n
  else invalid_arg "invalid host name"

let prepend ?hostname xs lbl =
  try Ok (prepend_exn ?hostname xs lbl) with
  | Invalid_argument e -> Error (`Msg e)

let drop_labels_exn ?(back = false) ?(amount = 1) t =
  let len = Array.length t - amount
  and start = if back then amount else 0
  in
  Array.sub t start len

let drop_labels ?back ?amount t =
  try Ok (drop_labels_exn ?back ?amount t) with
  | Invalid_argument _ -> Error (`Msg "couldn't drop labels")

let of_strings_exn ?(hostname = true) xs =
  let labels =
    (* we support both example.com. and example.com *)
    match List.rev xs with
    | ""::rst -> rst
    | rst -> rst
  in
  let t = Array.of_list labels in
  if check hostname t then t
  else invalid_arg "invalid host name"

let of_strings ?hostname xs =
  try Ok (of_strings_exn ?hostname xs) with
  | Invalid_argument e -> Error (`Msg e)

let of_string ?hostname s =
  of_strings ?hostname (String.cuts ~sep:"." s)

let of_string_exn ?hostname s =
  of_strings_exn ?hostname (String.cuts ~sep:"." s)

let of_array a = a

let to_array a = a

let to_strings ?(trailing = false) dn =
  let labels = Array.to_list dn in
  List.rev (if trailing then "" :: labels else labels)

let to_string ?trailing dn = String.concat ~sep:"." (to_strings ?trailing dn)

let canonical t =
  let str = to_string t in
  of_string_exn ~hostname:false (String.Ascii.lowercase str)

(*BISECT-IGNORE-BEGIN*)
let pp ppf xs = Fmt.string ppf (to_string xs)
(*BISECT-IGNORE-END*)

let compare_sub a b =
  String.compare (String.Ascii.lowercase a) (String.Ascii.lowercase b)

let compare_domain cmp_sub a b =
  let la = Array.length a in
  match compare la (Array.length b) with
  | 0 ->
    let rec cmp idx =
      if idx = la then 0
      else
        match cmp_sub (Array.get a idx) (Array.get b idx) with
        | 0 -> cmp (succ idx)
        | x -> x
    in
    cmp 0
  | x -> x

let compare = compare_domain compare_sub

let equal ?(case_sensitive = false) a b =
  let cmp = if case_sensitive then String.compare else compare_sub in
  compare_domain cmp a b = 0

let sub ~subdomain ~domain =
  let supl = Array.length domain in
  let rec cmp idx =
    if idx = supl then
      true
    else
      compare_sub (Array.get domain idx) (Array.get subdomain idx) = 0 &&
      cmp (succ idx)
  in
  if Array.length subdomain < supl then
    false
  else
    cmp 0

module Ordered = struct
  type t = s
  let compare = compare_domain compare_sub
end

type t = s

module Map = struct
  include Map.Make(Ordered)

  let find k m = try Some (find k m) with Not_found -> None
end

module Set = Set.Make(Ordered)
