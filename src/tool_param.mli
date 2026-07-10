(** Typed tool-parameter descriptors shared by JSON Schema generation and
    runtime argument decoding. *)

type 'a kind
type 'a t
type packed
type invalid_default = [ `Reject | `Use_default ]

val string : ?non_empty:bool -> unit -> string kind
(** [non_empty] is a runtime-only compatibility refinement: it deliberately does
    not add [minLength] to provider-facing schemas. *)

val string_enum : string list -> string kind
val boolean : bool kind
val string_array : ?min_items:int -> ?max_items:int -> unit -> string list kind
val required : name:string -> description:string -> 'a kind -> 'a t
val optional : name:string -> description:string -> 'a kind -> 'a option t

val defaulted :
  ?on_invalid:invalid_default ->
  name:string ->
  description:string ->
  default:'a ->
  'a kind ->
  'a t
(** [Use_default] preserves legacy parsers that treated an explicitly malformed
    value the same as an omitted value. [Reject] returns a parse error for
    malformed supplied values. Runtime defaults are not emitted as the JSON
    Schema [default] annotation. *)

val pack : 'a t -> packed
val object_schema : packed list -> Yojson.Safe.t
val parse : 'a t -> Yojson.Safe.t -> ('a, string) result
