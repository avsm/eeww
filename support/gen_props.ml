(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Extracts case data from the Unicode Character Database *)

let str = Format.sprintf
let exec = Filename.basename Sys.executable_name

let ucd_or_die inf = try
  let ic = if inf = "-" then stdin else open_in inf in
  let d = Uucd.decoder (`Channel ic) in
  match Uucd.decode d with
  | `Ok db -> db
  | `Error e ->
      let (l0, c0), (l1, c1) = Uucd.decoded_range d in
      Printf.eprintf "%s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e;
      exit 1
with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1

let process inf age alpha block break case case_map case_fold case_nfkc cjk
    func gc gen id name num script white =
  let inf = match inf with None -> "support/ucd.xml" | Some inf -> inf in
  let ucd = (Gen.log "Loading Unicode character database.\n"; ucd_or_die inf) in
  let generate pp f ucd = match f with
  | None -> ()
  | Some fn ->
      try
        let oc = if fn = "-" then stdout else Pervasives.open_out fn in
        try
          let ppf = Format.formatter_of_out_channel oc in
          pp ppf ucd;
          Format.pp_print_flush ppf ();
          close_out oc
        with Sys_error _ as e -> close_out oc; raise e
      with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1
  in
  Gen.log "Note: reported sizes do not take sharing into account.\n";
  generate Gen_age.pp_mod age ucd;
  generate Gen_alpha.pp_mod alpha ucd;
  generate Gen_block.pp_mod block ucd;
  generate Gen_break.pp_mod break ucd;
  generate Gen_case.pp_mod case ucd;
  generate Gen_case_map.pp_mod case_map ucd;
  generate Gen_case_fold.pp_mod case_fold ucd;
  generate Gen_case_nfkc.pp_mod case_nfkc ucd;
  generate Gen_cjk.pp_mod cjk ucd;
  generate Gen_func.pp_mod func ucd;
  generate Gen_gc.pp_mod gc ucd;
  generate Gen_gen.pp_mod gen ucd;
  generate Gen_id.pp_mod id ucd;
  generate Gen_name.pp_mod name ucd;
  generate Gen_num.pp_mod num ucd;
  generate Gen_script.pp_mod script ucd;
  generate Gen_white.pp_mod white ucd;
  ()

let main () =
  let usage = str
    "Usage: %s [OPTION]... [DBFILE]\n\
     \ Generates data modules from an Unicode character database XML file.\n\
     \ DBFILE defaults to support/ucd.xml\n\
     Options:" exec
  in
  let inf = ref None in
  let set_inf f =
    if !inf = None then inf := Some f else
    raise (Arg.Bad "only one Unicode character database file can be specified")
  in
  let age = ref None in
  let alpha = ref None in
  let block = ref None in
  let break = ref None in
  let case = ref None in
  let case_map = ref None in
  let case_fold = ref None in
  let case_nfkc = ref None in
  let cjk = ref None in
  let func = ref None in
  let gc = ref None in
  let gen = ref None in
  let id = ref None in
  let name = ref None in
  let num = ref None in
  let script = ref None in
  let white = ref None in
  let set r = Arg.String (fun s -> r := Some s) in
  let options = [
    "-age", set age, "<FILE> Support for the age property";
    "-alpha", set alpha, "<FILE> Support for the alphabetic property";
    "-block", set block, "<FILE> Support for block properties";
    "-break", set break, "<FILE> Support for break properties";
    "-case", set case, "<FILE> Support for case properties";
    "-case-map", set case_map, "<FILE> Support for case mappings";
    "-case-fold", set case_fold, "<FILE> Support for case folding";
    "-case-nfkc", set case_nfkc, "<FILE> Support for NFKC case folding";
    "-cjk", set cjk, "<FILE> Support for CJK properties";
    "-func", set func, "<FILE> Support for function and graph props";
    "-gc", set gc, "<FILE> Support for the general category property";
    "-gen", set gen, "<FILE> Support for general props";
    "-id", set id, "<FILE> Support for id properties";
    "-name", set name, "<FILE> Support for name properties";
    "-num", set num, "<FILE> Support for numeric properties";
    "-script", set script, "<FILE> Support for script properties";
    "-white", set white, "<FILE> Support for the white space property"; ]
  in
  Arg.parse (Arg.align options) set_inf usage;
  process !inf !age !alpha !block !break !case !case_map !case_fold
    !case_nfkc !cjk !func !gc !gen !id !name !num !script !white

let () = main ()

(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
