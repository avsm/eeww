(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

module type S = sig
  (* We go to some effort here to avoid having types directly refer to those in
     [Progress_engine] rather than going via the aliases. *)

  (** *)

  (** For example:

      See the [examples] directory for more complicated examples.

      First, we need some basic types: *)

  module Color = Ansi.Color

  module Ansi : sig
    include Ansi.Style with type color := Color.t
  end

  module Duration = Duration
  module Printer = Printer
  module Units = Units

  (** {1 Description} *)

  type 'a reporter := 'a -> unit
  (** A {i reporter} for values of type ['a]. In this library, each progress bar
      has its own reporting function. *)

  (** [Line] contains a DSL for defining custom progress bars. *)
  module Line : sig
    (** @inline *)
    include
      Line.S
        with type 'a t = 'a Line.t
         and type color := Color.t
         and type 'a printer := 'a Printer.t
  end

  (** [Multi] extends [Line] to multi-line layouts. *)
  module Multi : sig
    (** @inline *)
    include
      Multi.S with type 'a line := 'a Line.t and type 'a reporter := 'a reporter
  end

  val counter :
       total:int64
    -> ?style:[ `ASCII | `UTF8 | `Custom of string list ]
    -> ?message:string
    -> ?pp:int64 Printer.t
    -> unit
    -> int64 Line.t
  (** [counter ~total ()] is a progress bar of the form:

      {[
        <message?> <count?> MM:SS [########..............................]  XX%
      ]}

      where each reported value contributes cumulatively towards an eventual
      total of [total]. Optional parameters are as follows:

      - [?style] specifies whether to use a UTF-8 or an ASCII encoding for the
        progress bar. The UTF-8 encoding shows a higher resolution of progress,
        but may not be supported in all terminals. The default is [`ASCII].

      - [?pp] is used to pretty-print the [<count>] segment, if passed. For
        example, {!Units.bytes} can be used for totals measured in bytes. The
        default is to not display this segment. *)

  (** {1 Rendering} *)

  (** Configuration for progress bar rendering. *)
  module Config : sig
    type t

    val v :
         ?ppf:Format.formatter
      -> ?hide_cursor:bool
      -> ?persistent:bool
      -> ?max_width:int option
      -> ?min_interval:Duration.t option
      -> unit
      -> t
    (** @param ppf The formatter to use for rendering. Defaults to
        [Format.err_formatter].
        @param hide_cursor Whether or not to hide the terminal cursor (using the
        {{:https://en.wikipedia.org/wiki/ANSI_escape_code} [DECTCEM]} ANSI
        escape codes) during progress bar rendering. Defaults to [true]. *)

    val ( || ) : t -> t -> t
    (** Merge two config values, with settings from the left taking priority.
        That is, [a || b] contains the configuration of [a], with unset defaults
        taken from [b]. *)

    (** Provides the default values of each of the config parameters. *)
    module Default : sig
      val ppf : Format.formatter
      (** [ppf] is [Format.err_formatter]. *)

      val hide_cursor : bool
      (** [hide_cursor] is [true]. *)

      val persistent : bool
      (** [persistent] is [true]. *)

      val max_width : int option
      (** [max_width] is [None]. *)

      val min_interval : Duration.t option
      (** [min_interval] is 1/60th of a second. *)
    end
  end

  (** @inline *)
  include
    Renderer.S
      with type 'a reporter := 'a reporter
       and type 'a line := 'a Line.t
       and type ('a, 'b) multi := ('a, 'b) Multi.t
       and type config := Config.t
end

module type Progress_engine = sig
  module type S = S

  module Make (_ : Platform.S) : S
  module Integer = Integer
end

(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
  ————————————————————————————————————————————————————————————————————————————*)
