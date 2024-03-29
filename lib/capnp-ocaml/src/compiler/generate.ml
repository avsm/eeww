(******************************************************************************
 * capnp-ocaml
 *
 * Copyright (c) 2013-2014, Paul Pelzl
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)

module Uint64 = Stdint.Uint64

module List = Base.List
module String = Base.String
module Hashtbl = Base.Hashtbl
module Out_channel = Stdio.Out_channel

module PS   = GenCommon.PS
module C    = Capnp

module Context = GenCommon.Context
module Mode    = GenCommon.Mode


let sig_s_header ~context = [
  "[@@@ocaml.warning \"-27-32-37-60\"]";
  "";
  "type ro = Capnp.Message.ro";
  "type rw = Capnp.Message.rw";
  "";
  "module type S = sig";
  "  module MessageWrapper : Capnp.RPC.S";
  "  type 'cap message_t = 'cap MessageWrapper.Message.t";
  "  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t";
  "  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t";
  "";
] @ (List.concat_map context.Context.imports ~f:(fun import -> [
      "  module " ^ import.Context.schema_name ^ " : " ^
        import.Context.module_name ^ ".S with";
      "    module MessageWrapper = MessageWrapper";
      "";
    ]))

let sig_s_reader_header = [
  "";
  "  module Reader : sig";
  "    type array_t";
  "    type builder_array_t";
  "    type pointer_t = ro MessageWrapper.Slice.t option";
  "    val of_pointer : pointer_t -> 'a reader_t";
]

let sig_s_divide_reader_builder = [
  "  end";
  "";
  "  module Builder : sig";
  "    type array_t = Reader.builder_array_t";
  "    type reader_array_t = Reader.array_t";
  "    type pointer_t = rw MessageWrapper.Slice.t";
]

let sig_s_client_header = [
  "";
  "  module Client : sig";
]

let sig_s_divide_client_service = [
  "  end";
  "";
  "  module Service : sig";
]

let sig_s_service_footer = [
  "  end";
]

let sig_s_footer = [
  "  end";
  "end";
  "";
]


let functor_sig ~context ~rpc = [
  "module MakeRPC(MessageWrapper : Capnp.RPC.S) : sig";
  "  include S with module MessageWrapper = MessageWrapper"; ] @
  (List.concat_map context.Context.imports ~f:(fun import -> [
        "    and module " ^ import.Context.schema_name ^ " = " ^
          import.Context.module_name ^ ".MakeRPC(MessageWrapper)";
  ])) @ rpc @ [
  "end";
  "";
  "module Make(M : Capnp.MessageSig.S) : module type of MakeRPC(Capnp.RPC.None(M))";
]

let mod_functor_header = [
  "module MakeRPC(MessageWrapper : Capnp.RPC.S) = struct";
  "  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t";
  "  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t";
  "  module CamlBytes = Bytes";
]

let mod_header ~context = [
  "  let invalid_msg = Capnp.Message.invalid_msg";
  "";
  "  include Capnp.Runtime.BuilderInc.Make(MessageWrapper)";
  "";
  "  type 'cap message_t = 'cap MessageWrapper.Message.t";
  ""; ] @ (List.concat_map context.Context.imports ~f:(fun import -> [
      "  module " ^ import.Context.schema_name ^ " = " ^
          import.Context.module_name ^ ".MakeRPC(MessageWrapper)";
      "";
    ]))

let mod_reader_header = [
  "";
  "  module Reader = struct";
  "    type array_t = ro MessageWrapper.ListStorage.t";
  "    type builder_array_t = rw MessageWrapper.ListStorage.t";
  "    type pointer_t = ro MessageWrapper.Slice.t option";
  "    let of_pointer = RA_.deref_opt_struct_pointer";
  "";
]

let mod_divide_reader_builder = [
  "  end";
  "";
  "  module Builder = struct";
  "    type array_t = Reader.builder_array_t";
  "    type reader_array_t = Reader.array_t";
  "    type pointer_t = rw MessageWrapper.Slice.t";
  "";
]

let mod_divide_builder_client = [
  "  end";
  "";
  "  module Client = struct";
]

let mod_divide_client_service = [
  "  end";
  "";
  "  module Service = struct";
]

let mod_footer = [
  "  end";
]

let mod_functor_footer = [
  "  module MessageWrapper = MessageWrapper";
  "end";
  "";
  "module Make(M:Capnp.MessageSig.S) = MakeRPC(Capnp.RPC.None(M))";
]


let ml_filename filename =
  let module_name = GenCommon.make_legal_module_name filename in
  String.uncapitalize (module_name ^ ".ml")

let mli_filename filename =
  let module_name = GenCommon.make_legal_module_name filename in
  String.uncapitalize (module_name ^ ".mli")


let string_of_lines lines =
  (String.concat ~sep:"\n" lines) ^ "\n"


let calculate_positions ~nodes ~requested_file_node =
  let positions = Hashtbl.Poly.create ~size:(Hashtbl.Poly.length nodes) () in
  let pos = ref 0 in
  let rec scan node =
    let id = PS.Node.id_get node in
    Hashtbl.Poly.add_exn positions ~key:id ~data:!pos;
    pos := succ !pos;
    let children =
      GenCommon.child_ids_of node
      |> List.map ~f:(Hashtbl.Poly.find_exn nodes)
    in
    List.iter children ~f:scan
  in
  scan requested_file_node;
  positions

let compile
    (request : PS.CodeGeneratorRequest.t)
  : unit =
  let nodes_table = Hashtbl.Poly.create () in
  let nodes = PS.CodeGeneratorRequest.nodes_get request in
  let () = C.Array.iter nodes ~f:(fun node ->
      Hashtbl.set nodes_table ~key:(PS.Node.id_get node) ~data:node)
  in
  let requested_files = PS.CodeGeneratorRequest.requested_files_get request in
  C.Array.iter requested_files ~f:(fun requested_file ->
    let open PS.CodeGeneratorRequest in
    let requested_file_id = RequestedFile.id_get requested_file in
    let requested_file_node = Hashtbl.find_exn nodes_table requested_file_id in
    let requested_filename = RequestedFile.filename_get requested_file in
    let imports = C.Array.map_list (RequestedFile.imports_get requested_file)
        ~f:(fun import ->
          let import_id = RequestedFile.Import.id_get import in
          let schema_filename = RequestedFile.Import.name_get import in
          let module_name = GenCommon.make_legal_module_name schema_filename in {
            Context.id = import_id;
            Context.schema_name = module_name ^ "_" ^
                (Uint64.to_string import_id);
            Context.module_name = module_name
          })
    in
    let context_unfiltered = {
      Context.nodes     = nodes_table;
      Context.imports   = imports;
      Context.positions = calculate_positions ~nodes:nodes_table ~requested_file_node;
    } in
    let context = GenCommon.filter_interesting_imports
        ~context:context_unfiltered requested_file_node
    in
    let sig_unique_enums =
      GenCommon.apply_indent ~indent:"  "
        (GenCommon.collect_unique_enums ~is_sig:true ~context requested_file_node)
    in
    let mod_unique_enums =
      GenCommon.apply_indent ~indent:"  "
        (GenCommon.collect_unique_enums ~is_sig:false ~context requested_file_node)
    in
    let sig_s =
      (sig_s_header ~context) @
      sig_unique_enums @
      sig_s_reader_header @
      (GenCommon.apply_indent ~indent:"    "
        (GenSignatures.generate_node ~suppress_module_wrapper:true ~context
           ~scope:[] ~mode:Mode.Reader ~node_name:requested_filename
           requested_file_node)) @
      sig_s_divide_reader_builder @
      (GenCommon.apply_indent ~indent:"    "
        (GenSignatures.generate_node ~suppress_module_wrapper:true ~context
           ~scope:[] ~mode:Mode.Builder ~node_name:requested_filename
           requested_file_node)) @
      sig_s_footer
    in
    let sig_file_content =
      let rpc =
        sig_s_client_header @
        (GenCommon.apply_indent ~indent:"    "
           (GenSignatures.generate_clients ~suppress_module_wrapper:true ~context
              ~scope:[] ~node_name:requested_filename
              requested_file_node)) @
        sig_s_divide_client_service @
        (GenCommon.apply_indent ~indent:"    "
           (GenSignatures.generate_services ~suppress_module_wrapper:true ~context
              ~scope:[] ~node_name:requested_filename
              requested_file_node)) @
        sig_s_service_footer
      in
      string_of_lines (sig_s @ (functor_sig ~context ~rpc))
    in
    let mod_shared_content =
      let defaults_context =
        GenModules.build_defaults_context ~context ~node_name:requested_filename
        requested_file_node
      in
      let reader_body =
        GenCommon.apply_indent ~indent:"    "
          (GenModules.generate_node ~suppress_module_wrapper:true ~context
             ~scope:[] ~mode:Mode.Reader ~node_name:requested_filename
             requested_file_node)
      in
      let builder_body =
        GenCommon.apply_indent ~indent:"    "
          (GenModules.generate_node ~suppress_module_wrapper:true ~context
             ~scope:[] ~mode:Mode.Builder ~node_name:requested_filename
             requested_file_node)
      in
      let builder_defaults = GenCommon.apply_indent ~indent:"  "
          (Defaults.gen_builder_defaults defaults_context)
      in
      let reader_defaults = GenCommon.apply_indent ~indent:"  "
          (Defaults.gen_reader_defaults defaults_context)
      in
      let client_body =
        GenCommon.apply_indent ~indent:"    "
          (GenModules.generate_clients ~suppress_module_wrapper:true ~context
             ~scope:[] ~node_name:requested_filename
             requested_file_node)
      in
      let service_body =
        GenCommon.apply_indent ~indent:"    "
          (GenModules.generate_services ~suppress_module_wrapper:true ~context
             ~scope:[] ~node_name:requested_filename
             requested_file_node)
      in
      builder_defaults @
      (mod_header ~context) @
      mod_unique_enums @
      reader_defaults @
      mod_reader_header @
      reader_body @
      mod_divide_reader_builder @
      builder_body @
      mod_divide_builder_client @
      client_body @
      mod_divide_client_service @
      service_body @
      mod_footer
    in
    let mod_file_content =
      string_of_lines (
        sig_s @
        mod_functor_header @
        mod_shared_content @
        mod_functor_footer)
    in
    let () = Out_channel.with_file (mli_filename requested_filename)
        ~f:(fun chan -> Out_channel.output_string chan sig_file_content)
    in
    let () = Out_channel.with_file (ml_filename requested_filename)
        ~f:(fun chan -> Out_channel.output_string chan mod_file_content)
    in
    let () = Printf.printf "%s --> %s %s\n"
        requested_filename
        (mli_filename requested_filename)
        (ml_filename requested_filename)
    in
    ())


