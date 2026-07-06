(* Shared types for structured pipeline definition, execution, and reporting. *)

type input_def = {
  input_type : string;
  description : string;
  required : bool;
  default : string option;
}

type step_kind =
  | Prompt_step of {
      prompt : string;
      system_prompt : string option;
      model : string option;
      output_schema : Yojson.Safe.t;
      max_retries : int;
    }
  | Pipeline_step of { pipeline : string; input_map : (string * string) list }
  | Agent_step of {
      task : string;
      model : string option;
      max_turns : int option;
    }

type step = { name : string; kind : step_kind }

type pipeline_def = {
  name : string;
  version : string;
  description : string;
  inputs : (string * input_def) list;
  steps : step list;
  source_path : string;
}

type step_result = {
  step_name : string;
  output_json : Yojson.Safe.t;
  output_raw : string;
  model_used : string;
  attempts : int;
  elapsed_s : float;
  tokens : (int * int) option;
}

type run_status = Running | Completed | Failed of string | Cancelled

type run = {
  run_id : int;
  pipeline_name : string;
  pipeline_version : string;
  inputs : (string * string) list;
  step_results : step_result list;
  status : run_status;
  started_at : string;
  finished_at : string option;
}
