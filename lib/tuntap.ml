(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type kind = Tap | Tun

external opentun_stub : string -> kind -> bool -> int
  -> int -> int -> Unix.file_descr * string = "tun_opendev_byte" "tun_opendev"
external get_macaddr : string -> string = "get_macaddr"
external get_mtu : string -> int = "get_mtu"
external set_ipv4 : string -> string -> string -> unit = "set_ipv4"
external set_up_and_running : string -> unit = "set_up_and_running"

external get_ifnamsiz : unit -> int = "get_ifnamsiz"

let open_ kind ?(pi=false) ?persist
    ?(user = -1) ?(group = -1) ?(devname="") () =
  let persist_int = match persist with
    | None -> -1
    | Some false -> 0
    | Some true -> 1 in
  opentun_stub devname kind pi persist_int user group

let opentun = open_ Tun
let opentap = open_ Tap

(* Closing is just opening an existing device in non-persistent
   mode *)
let closetun devname = ignore (opentun ~devname ~persist:false ())
let closetap devname = ignore (opentap ~devname ~persist:false ())

let set_ipv4 ?(netmask=Ipaddr.V4.Prefix.global) devname v4addr =
  let open Ipaddr.V4 in
  set_ipv4 devname (to_bytes v4addr) (to_bytes (Prefix.netmask netmask))

let get_macaddr iface = Macaddr.of_bytes_exn (get_macaddr iface)

module Opt = struct
  let (>|=) x f = match x with Some v -> Some (f v) | None -> None
  let run = function
    | Some x -> x
    | None -> raise Not_found
end

module Struct_ifaddrs = struct
  type t = {
    name: string;
    sa_family: int;
    addr: string option;
    mask: string option;
    brd:  string option;
  }

  type ptr_t

  external getifaddrs_stub : unit -> ptr_t option = "getifaddrs_stub"
  external freeifaddrs_stub : ptr_t -> unit = "freeifaddrs_stub"
  external iface_get : ptr_t -> t = "iface_get"
  external iface_next : ptr_t -> ptr_t option = "iface_next"

  let to_t' t =
    let open Ipaddr in
    let open Opt in
    match t.sa_family with
    | 0 ->
      let addr = t.addr >|= fun v -> (V4.of_bytes_exn v) in
      let nmask = t.mask >|= fun v -> (V4.of_bytes_exn v) in
      Some (t.name, `V4 ((run addr), V4.Prefix.(of_netmask (run nmask) (run addr))))
    | 1 ->
      let addr = t.addr >|= fun v -> (V6.of_bytes_exn v) in
      let nmask = t.mask >|= fun v -> (V6.of_bytes_exn v) in
      Some (t.name, `V6 ((run addr), V6.Prefix.(of_netmask (run nmask) (run addr))))
    | _ -> None
end

let getifaddrs () =
  let open Struct_ifaddrs in
  match getifaddrs_stub () with
  | None -> []
  | Some start ->
    let rec loop acc ptr =
      let acc = match to_t' (iface_get ptr) with
        | None -> acc
        | Some t' -> t'::acc
      in
      match iface_next ptr with
      | None -> freeifaddrs_stub start; acc
      | Some p -> loop acc p
    in
    loop [] start

let filter_map f l =
  List.fold_left (fun a v -> match f v with Some v' -> v'::a | None -> a) [] l

let getifaddrs_v4 () =
  filter_map (function (ifn, `V4 a) -> Some (ifn, a)  | _ -> None) @@
  getifaddrs ()

let getifaddrs_v6 () =
  filter_map (function (ifn, `V6 a) -> Some (ifn, a ) | _ -> None) @@
  getifaddrs ()

let addrs_of_ifname ifname =
  filter_map (fun (ifn, a) -> if ifn = ifname then Some a else None) @@
  getifaddrs ()

let v4_of_ifname ifname =
  filter_map (fun (ifn, a) -> if ifn = ifname then Some a else None) @@
  getifaddrs_v4 ()

let v6_of_ifname ifname =
  filter_map (fun (ifn, a) -> if ifn = ifname then Some a else None) @@
  getifaddrs_v6 ()
