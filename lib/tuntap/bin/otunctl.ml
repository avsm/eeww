(*
 * Copyright (c) 2013 Vincent Bernardoff <vb@luminar.eu.org>
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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

open Cmdliner

type optype = [ `Add | `Del ]

let optype_parser = function
  | "add" -> `Ok `Add
  | "del" -> `Ok `Del
  | _     -> `Error "invalid argument"

let optype_printer f = function
  | `Add -> Format.pp_print_string f "add"
  | `Del -> Format.pp_print_string f "del"
  | _    -> Format.pp_print_string f "invalid argument, must be \"add\" or \"del\""

let optype_converter = optype_parser, optype_printer

type mode = [ `Tun | `Tap ]

let mode_parser = function
  | "tun" -> `Ok `Tun
  | "tap" -> `Ok `Tap
  | _     -> `Error "invalid argument, must be \"tun\" or \"tap\""

let mode_printer f = function
  | `Tun -> Format.pp_print_string f "tun"
  | `Tap -> Format.pp_print_string f "tap"
  | _    -> Format.pp_print_string f "invalid argument, must be \"tun\" or \"tap\""

let mode_converter = mode_parser, mode_printer

let optype = Arg.(required & pos 0 (some optype_converter) None
                  & info ~docv:"OPTYPE"
                    ~doc:"Type of operation, either \"add\" or \"del\"." [])

let dev = Arg.(required & pos 1 (some string) None & info ~docv:"PHYS_DEV"
                 ~doc:"Name of the virtual device, typically \"tap0\" or \"tun0\"." [])

let mode = Arg.(required & pos 2 (some mode_converter) None & info ~docv:"MODE"
                  ~doc:"Mode of the virtual device, MUST be \"tun\" or \"tap\"." [])

let user = Arg.(value & opt (some int) None & info ~docv:"USER"
                  ~doc:"Specify the uid that owns the device." ["user"; "u"])

let group = Arg.(value & opt (some int) None & info ~docv:"GROUP"
                   ~doc:"Specify the gid that owns the device." ["group"; "g"])

let pi =
  let doc = "Prepend the protocol information header in every received frame" in
  Arg.(value & flag & info ~docv:"pi" ~doc ["pi"])

let tunctl optype devname mode user group pi =
  match optype, mode with
    | `Add, `Tap -> let _, devname = Tuntap.opentap ~persist:true ~devname ?user ?group ~pi () in
                    Printf.printf "OK, %s, hwaddr %s\n%!" devname
                      Tuntap.(Macaddr.to_string (get_macaddr devname))
    | `Add, `Tun -> let _, devname = Tuntap.opentun ~persist:true ~devname ?user ?group ~pi () in
                    Printf.printf "OK, %s, hwaddr %s\n%!" devname
                      Tuntap.(Macaddr.to_string (get_macaddr devname))
    | `Del, `Tap -> Tuntap.closetap devname
    | `Del, `Tun -> Tuntap.closetun devname

let cmd =
  let doc = "Create and destroy virtual interfaces." in
  Term.(pure tunctl $ optype $ dev $ mode $ user $ group $ pi),
  Term.info "otunctl" ~version:"1.0.0" ~doc

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
