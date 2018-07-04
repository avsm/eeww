(* (c) 2017 Hannes Mehnert, all rights reserved *)

type t
(** The type of a domain name, a sequence of labels separated by dots.  Each
    label may contain any bytes. The length of each label may not exceed 63
    charactes.  The total length of a domain name is limited to 253 (byte
    representation is 255), but other protocols (such as SMTP) may apply even
    smaller limits.  A domain name label is case preserving, comparison is done
    in a case insensitive manner.  Every [t] is a fully qualified domain name,
    its last label is the [root] label.  Input ([of_string]) does not require
    a trailing dot ([of_strings] does not require a trailing empty string).
    Output ([to_string], [pp]) omit the trailing dot.

    The invariants on the length of domain names are preserved throughout the
    module - no [t] will exist which violates these.

    The specification of domain names originates from
    {{:https://tools.ietf.org/html/rfc1035}RFC 1035}. *)

(** {2 Constructor} *)

val root : t
(** [root] is the root domain ("."), the empty label. *)

(** {2 String representation} *)

val of_string : ?hostname:bool -> string -> (t, [> `Msg of string ]) result
(** [of_string ~hostname name] is either [t], the domain name, or an error if
    the provided [name] is not a valid domain name.  If [hostname] is provided
    and [true] (the default), the contents is additionally checked for being a
    valid hostname using {!is_hostname}.  A potential trailing '.' is ignored,
    [equal (of_string_exn "example.com") (of_string_exn "example.com.") = true] *)

val of_string_exn : ?hostname:bool -> string -> t
(** [of_string_exn ~hostname name] is [t], the domain name.  If [hostname] is
    provided and [true] (the default), the contents is additionally checked for
    being a valid hostname using {!is_hostname}.

    @raise Invalid_argument if [name] is not a valid domain name. *)

val to_string : t -> string
(** [to_string t] is [String.concat ~sep:"." (to_strings t)], a human-readable
    representation of [t]. *)

(** {2 Predicates and basic operations} *)

val canonical : t -> t
(** [canonical t] is [t'], the canonical domain name, as specified in RFC 4034
    (and 2535): all characters are lowercase. *)

val is_hostname : t -> bool
(** [is_hostname t] is [true] if [t] is a hostname: the contents of the domain
    name is limited: each label may start with a digit or letter, followed by
    digits, letters, or hyphens. *)

val is_service : t -> bool
(** [is_service t] is [true] if [t] is a service label: the first label is a
    service name (containing of letters, digits, hyphens) prefixed with "_".
    The service name may not contain a hyphen following another hyphen, no hypen
    at the beginning or end, and must contain at least one letter.  The total
    length must be between 1 and at most 15 characters.  The second label is the
    protocol, prefixed by "_" (at the moment, tcp, udp, or sctp), and the
    remaining must be a host name. *)

val sub : subdomain:t -> domain:t -> bool
(** [sub ~subdomain ~domain] is [true] if [subdomain] contains any labels
    prepended to [domain]: [foo.bar.com] is a subdomain of [bar.com] and of
    [com], [sub ~subdomain:x ~domain:root] is true for all [x]. *)

(** {2 Label addition and removal} *)
val prepend : ?hostname:bool -> t -> string -> (t, [> `Msg of string ]) result
(** [prepend ~hostname name pre] is either [t], the new domain name, or an
    error.  If [hostname] is provided and [true] (the default), the resulting
    domain name is checked for being a valid host name using {!is_hostname}. *)

val prepend_exn : ?hostname:bool -> t -> string -> t
(** [prepend_exn ~hostname name pre] is [t], the new domain name.

    @raise Invalid_argument if [pre] is not a valid domain name. If
    [hostname] is provided and [true] (the default), the contents is
    additionally checked for being a valid host name using {!is_hostname}. *)

val drop_labels : ?back:bool -> ?amount:int -> t -> (t, [> `Msg of string ]) result
(** [drop_labels ~back ~amount t] is either [t], a domain name with [amount]
    (defaults to 1) labels dropped from the beginning (unless [back] is provided
    and [true], defaults to [false]).  [drop_labels] applied to [foo.com] is
    [com]. *)

val drop_labels_exn : ?back:bool -> ?amount:int -> t -> t
(** [drop_labels_exn ~back ~amount t] is either [t], a domain name with [amount]
    (defaults to 1) labels dropped from the beginning (unless [back] is provided
    and [true], defaults to [false]).  [drop_labels] applied to [foo.com] is
    [com].

    @raise Invalid_argument if there are not sufficient labels. *)

(** {2 Comparison} *)

val equal : ?case_sensitive:bool -> t -> t -> bool
(** [equal ~case t t'] is [true] if all labels of [t] and [t'] are equal.
    If [case_sensitive] is provided and [true], the cases of the labels are
    respected (default: [false]). *)

val compare : t -> t -> int
(** [compare t t'] compares the domain names [t] and [t'] using a case
    insensitive string comparison. *)

val compare_sub : string -> string -> int
(** [compare_sub t t'] compares the labels [t] and [t'] using a case
    insensitive string comparison. *)

(** {2 Collections} *)

module Map : sig
  include Map.S with type key = t

  (** [find key t] is [Some a] where a is the binding of [key] in [t]. [None] if
      the [key] is not present. *)
  val find : key -> 'a t -> 'a option
end
(** The module of a domain name map *)

module Set : Set.S with type elt = t
(** The module of a domain name set *)

(** {2 String list representation} *)

val of_strings : ?hostname:bool -> string list -> (t, [> `Msg of string ]) result
(** [of_strings ~hostname labels] is either [t], a domain name, or an error if
    the provided [labels] violate domain name constraints.  If [hostname] is
    provided and [true] (the default), the labels are additionally checked for
    being a valid hostname using {!is_hostname}.  A potential trailing empty
    label is ignored. *)

val of_strings_exn : ?hostname:bool -> string list -> t
(** [of_strings_exn ~hostname labels] is [t], a domain name.

    @raise Invalid_argument if [labels] are not a valid domain name. If
    [hostname] is provided and [true] (the default), the labels are
    additionally checked for being a valid hostname using
    {!is_hostname}. *)

val to_strings : t -> string list
(** [to_strings t] is the list of labels of [t]. *)

(** {2 Pretty printer} *)

val pp : t Fmt.t
(** [pp ppf t] pretty prints the domain name [t] on [ppf]. *)

(**/**)
(* exposing internal structure, used by udns (but could as well use Obj.magic *)

val of_array : string array -> t
(** [of_array a] is [t], a domain name from [a], an array containing a reversed
    domain name. *)

val to_array : t -> string array
(** [to_array t] is [a], an array containing the reversed domain name of [t]. *)
