type wizard_state = {
  profile_id : string;
  display_name : string;
  model : string;
  system_prompt : string;
  max_tool_iterations : int;
  allowed_tools : string list;
  denied_tools : string list;
  access_bundle_ids : string list;
  memory_scope_kind : string;
  memory_scope_key : string;
  token_limit : int;
  cost_limit_usd : float;
  budget_reset_period : string;
  connector_type : string;
  connector_room : string;
  connector_active : bool;
}

type plan_item = { category : string; action : string; details : string }
type readiness_check = { name : string; passed : bool; message : string }

(** Status of a single config item when comparing desired vs current state. *)
type rerun_status =
  | Changed  (** Item differs from current config; will be updated. *)
  | Already_valid  (** Item matches current config; no action needed. *)
  | Blocked  (** Item cannot be applied due to missing dependencies. *)
  | Manual_repair  (** Item needs human intervention to resolve. *)

type rerun_item = {
  category : string;
  field : string;
  status : rerun_status;
  details : string;
}
(** A single entry in a rerun report. *)

let default_state : wizard_state =
  {
    profile_id = "";
    display_name = "";
    model = "openai:gpt-5.4";
    system_prompt = "";
    max_tool_iterations = 25;
    allowed_tools = [];
    denied_tools = [];
    access_bundle_ids = [];
    memory_scope_kind = "room";
    memory_scope_key = "";
    token_limit = 0;
    cost_limit_usd = 0.0;
    budget_reset_period = "monthly";
    connector_type = "teams";
    connector_room = "";
    connector_active = true;
  }
