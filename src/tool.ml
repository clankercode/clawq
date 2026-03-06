type risk_level = Low | Medium | High

type t = {
  name : string;
  description : string;
  parameters_schema : Yojson.Safe.t;
  invoke : Yojson.Safe.t -> string Lwt.t;
  risk_level : risk_level;
  deferred : bool;
}
