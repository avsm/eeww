(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

type t =
  [ `Cc
  | `Cf
  | `Cn
  | `Co
  | `Cs
  | `Ll
  | `Lm
  | `Lo
  | `Lt
  | `Lu
  | `Mc
  | `Me
  | `Mn
  | `Nd
  | `Nl
  | `No
  | `Pc
  | `Pd
  | `Pe
  | `Pf
  | `Pi
  | `Po
  | `Ps
  | `Sc
  | `Sk
  | `Sm
  | `So
  | `Zl
  | `Zp
  | `Zs ]

let pp ppf c = Format.fprintf ppf "%s" begin match c with
  | `Cc -> "Cc"
  | `Cf -> "Cf"
  | `Cn -> "Cn"
  | `Co -> "Co"
  | `Cs -> "Cs"
  | `Ll -> "Ll"
  | `Lm -> "Lm"
  | `Lo -> "Lo"
  | `Lt -> "Lt"
  | `Lu -> "Lu"
  | `Mc -> "Mc"
  | `Me -> "Me"
  | `Mn -> "Mn"
  | `Nd -> "Nd"
  | `Nl -> "Nl"
  | `No -> "No"
  | `Pc -> "Pc"
  | `Pd -> "Pd"
  | `Pe -> "Pe"
  | `Pf -> "Pf"
  | `Pi -> "Pi"
  | `Po -> "Po"
  | `Ps -> "Ps"
  | `Sc -> "Sc"
  | `Sk -> "Sk"
  | `Sm -> "Sm"
  | `So -> "So"
  | `Zl -> "Zl"
  | `Zp -> "Zp"
  | `Zs -> "Zs"
  end

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
