type risk_level = Low | Medium | High

type invoke_context = {
  session_key : string option;
  send_progress : (string -> unit Lwt.t) option;
}

type invoke_stream =
  ?context:invoke_context ->
  on_output_chunk:(string -> unit Lwt.t) ->
  Yojson.Safe.t ->
  string Lwt.t

type t = {
  name : string;
  description : string;
  parameters_schema : Yojson.Safe.t;
  invoke : ?context:invoke_context -> Yojson.Safe.t -> string Lwt.t;
  invoke_stream : invoke_stream option;
  risk_level : risk_level;
  deferred : bool;
}

let default_context = { session_key = None; send_progress = None }
