(* Strict tool-use preparation.
   OpenAI and Anthropic enforce [required] only when a tool is marked strict.
   Preparation happens at the request boundary for those flavors. Stored
   schemas and Gemini/Ollama/Cohere payloads stay unchanged.

   Optional properties become null unions and join [required], which is what
   OpenAI strict mode accepts. Invoke paths already treat null as unset.
   [command] and any other originally required field stay non-null. *)

let max_strict_tools = 20
let max_union_params = 16
let default_allowlist = [ "bash" ]
let rejected : (string, unit) Hashtbl.t = Hashtbl.create 8

let cache_key ~provider_name ~(base_url : string option) ~model =
  provider_name ^ "\n" ^ Option.value ~default:"" base_url ^ "\n" ^ model

let supported key = not (Hashtbl.mem rejected key)
let mark_unsupported key = Hashtbl.replace rejected key ()
let reset_cache () = Hashtbl.clear rejected
let contains_ci hay needle = String_util.contains_ci hay needle

let mentions_strict_unsupported body =
  contains_ci body "strict"
  && (contains_ci body "not support"
     || contains_ci body "unsupported"
     || contains_ci body "unknown parameter"
     || contains_ci body "unrecognized")

let is_strict_rejection body =
  contains_ci body "additionalproperties"
  || contains_ci body "additional_properties"
  || contains_ci body "invalid schema for function"
  || contains_ci body "schema is too complex"
  || contains_ci body "too complex for compilation"
  || contains_ci body "does not support strict"
  || contains_ci body "do not support strict"
  || contains_ci body "doesn't support strict"
  || contains_ci body "strict function tools are not supported"
  || mentions_strict_unsupported body
  || (contains_ci body "strict" && contains_ci body "schema")
  || (contains_ci body "strict" && contains_ci body "parallel")
  || contains_ci body "strict"
     && contains_ci body "extra inputs are not permitted"

let request_sent_strict body =
  String_util.contains body "\"strict\":true"
  || String_util.contains body "\"strict\": true"

let should_retry_without_strict ~request_body ~status ~response_body =
  status = 400
  && request_sent_strict request_body
  && is_strict_rejection response_body

let assoc_replace fields key value =
  (key, value) :: List.remove_assoc key fields

let string_list = function
  | `List items ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

let has_null = function
  | `List items ->
      List.exists (function `String "null" -> true | _ -> false) items
  | `String "null" -> true
  | _ -> false

let with_null typ =
  if has_null typ then typ
  else
    match typ with
    | `String t -> `List [ `String t; `String "null" ]
    | `List items -> `List (items @ [ `String "null" ])
    | other -> other

let wrap_optional_property prop =
  match prop with
  | `Assoc fields -> (
      match List.assoc_opt "type" fields with
      | Some typ -> Some (`Assoc (assoc_replace fields "type" (with_null typ)))
      | None -> None)
  | _ -> None

let normalize_parameters schema =
  match schema with
  | `Assoc fields -> (
      match List.assoc_opt "properties" fields with
      | Some (`Assoc props) -> (
          let original_required =
            string_list
              (List.assoc_opt "required" fields
              |> Option.value ~default:(`List []))
          in
          let rec wrap acc unions = function
            | [] -> Some (List.rev acc, unions)
            | (name, prop) :: rest -> (
                if List.mem name original_required then
                  wrap ((name, prop) :: acc) unions rest
                else
                  match wrap_optional_property prop with
                  | None -> None
                  | Some wrapped ->
                      wrap ((name, wrapped) :: acc) (unions + 1) rest)
          in
          match wrap [] 0 props with
          | None -> None
          | Some (props, unions) ->
              let extras =
                List.filter_map
                  (fun (name, _) ->
                    if List.mem name original_required then None else Some name)
                  props
              in
              let required = original_required @ extras in
              let fields = assoc_replace fields "properties" (`Assoc props) in
              let fields =
                assoc_replace fields "required"
                  (`List (List.map (fun s -> `String s) required))
              in
              let fields =
                assoc_replace fields "additionalProperties" (`Bool false)
              in
              Some (`Assoc fields, unions))
      | _ -> None)
  | _ -> None

let function_name = function
  | `Assoc fields -> (
      match List.assoc_opt "function" fields with
      | Some (`Assoc fn) -> (
          match List.assoc_opt "name" fn with
          | Some (`String name) -> Some name
          | _ -> None)
      | _ -> None)
  | _ -> None

let set_function_fields item updates =
  match item with
  | `Assoc fields -> (
      match List.assoc_opt "function" fields with
      | Some (`Assoc fn) ->
          let fn =
            List.fold_left
              (fun acc (key, value) -> assoc_replace acc key value)
              fn updates
          in
          Some (`Assoc (assoc_replace fields "function" (`Assoc fn)))
      | _ -> None)
  | _ -> None

let prepare_openai_tools ?(enable = true) ?(allowlist = default_allowlist)
    ?(max_tools = max_strict_tools) ?(max_unions = max_union_params) = function
  | `List tools when enable ->
      let remaining_tools = ref max_tools in
      let remaining_unions = ref max_unions in
      `List
        (List.map
           (fun tool ->
             match function_name tool with
             | Some name when List.mem name allowlist -> (
                 match tool with
                 | `Assoc fields -> (
                     match List.assoc_opt "function" fields with
                     | Some (`Assoc fn) -> (
                         match List.assoc_opt "parameters" fn with
                         | Some parameters -> (
                             match normalize_parameters parameters with
                             | Some (parameters, unions)
                               when unions <= !remaining_unions
                                    && !remaining_tools > 0 ->
                                 remaining_tools := !remaining_tools - 1;
                                 remaining_unions := !remaining_unions - unions;
                                 Option.value ~default:tool
                                   (set_function_fields tool
                                      [
                                        ("parameters", parameters);
                                        ("strict", `Bool true);
                                      ])
                             | _ -> tool)
                         | _ -> tool)
                     | _ -> tool)
                 | _ -> tool)
             | _ -> tool)
           tools)
  | other -> other
