(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp_block ppf b = Gen.pp ppf "`%a" Uucp_block_base.pp b    
let pp_block_option ppf = function
| None -> Gen.pp ppf "None"
| Some b -> Gen.pp ppf "@[<1>(Some@ %a)@]" pp_block b

let block_prop ucd u = match Gen.ucd_get ucd u Uucd.block with 
| `High_Surrogates -> assert false
| `Low_Surrogates -> assert false 
| `High_PU_Surrogates -> assert false
| #Uucp_block_base.t as b -> b

let pp_block_prop ppf ucd = 
  let size v = 0 in
  let prop u = block_prop ucd u in
  Gen.pp_prop_rmap ppf prop "block" "Uucp_block_base.t" pp_block 
    ~default:`NB size

let pp_blocks ppf ucd = 
  let ranges = Gen.prop_find_ranges (block_prop ucd) in 
  let not_nb (`R (_, _, b)) = b <> `NB in
  let ranges = List.find_all not_nb ranges in
  let pp_block ppf (`R (is,ie,b)) = 
    Gen.pp ppf "@[<1>(%a,@,(%d,@,%d))@]" pp_block b is ie 
  in
  Gen.pp ppf "@[let block_list : (Uucp_block_base.t * (int * int)) list =\
              @\n %a@]@\n"
    (Gen.pp_list pp_block) ranges
 
let pp_props ppf ucd =
  pp_block_prop ppf ucd;
  pp_blocks ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd
    
(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
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
