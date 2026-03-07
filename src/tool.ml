type risk_level = Low | Medium | High

type invoke_stream =
  on_output_chunk:(string -> unit Lwt.t) -> Yojson.Safe.t -> string Lwt.t

type t = {
  name : string;
  description : string;
  parameters_schema : Yojson.Safe.t;
  invoke : Yojson.Safe.t -> string Lwt.t;
  invoke_stream : invoke_stream option;
  risk_level : risk_level;
  deferred : bool;
}
