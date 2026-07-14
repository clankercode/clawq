(** B778: Pre-flight validation for room-scoped cron jobs.

    Room-scoped sessions (teams:/slack:/discord:/telegram:/…) need a room
    profile binding before tools like [room_memory_*] can operate. Scheduling a
    cron against an unbound room causes the agent to loop on the same
    configuration error every fire.

    Shared by CLI ([command_bridge_cron]) and slash ([format_cron]) so both
    surfaces reject (or warn) consistently. Use [~force:true] / [--force] to
    bypass the hard gate for advanced operators. *)

type validation = {
  warnings : string list;
      (** Soft warnings (e.g. missing CLAWQ_PRINCIPAL_ID for github-oriented
          jobs). *)
}

let empty_validation = { warnings = [] }

(** Connectors whose session keys identify chat rooms that participate in room
    profile binding / room memory. Worker keys ([cron:], [chat], …) are not
    room-scoped. *)
let room_scoped_prefixes =
  [ "teams:"; "slack:"; "discord:"; "telegram:"; "matrix:"; "dingtalk:" ]

let is_room_scoped_session_key (session_key : string) : bool =
  let key = String.lowercase_ascii (String.trim session_key) in
  List.exists (fun p -> String.starts_with ~prefix:p key) room_scoped_prefixes

let contains_ci ~haystack ~needle =
  let hay = String.lowercase_ascii haystack in
  let nee = String.lowercase_ascii needle in
  let hlen = String.length hay in
  let nlen = String.length nee in
  let rec loop i =
    if nlen = 0 then true
    else if i + nlen > hlen then false
    else if String.sub hay i nlen = nee then true
    else loop (i + 1)
  in
  loop 0

(** Heuristic: job name/message implies GitHub room tools or github_account. *)
let github_oriented ~name ~message =
  let blob = name ^ " " ^ message in
  List.exists
    (fun kw -> contains_ci ~haystack:blob ~needle:kw)
    [
      "github";
      "github_room";
      "github_account";
      "repo-monitor";
      "repo_monitor";
      "gh_";
      "pull request";
      "pull-request";
      "pr review";
      "pr-monitor";
    ]

let config_has_binding (config : Runtime_config.t) ~session_key =
  match Runtime_config.resolve_room_profile config ~session_key with
  | Some _ -> true
  | None -> false

let db_has_binding ~db ~session_key =
  try Agent_profile.session_has_room_profile_binding ~db session_key
  with _ -> false

let has_room_profile_binding ?config ?db ~session_key () =
  let from_config =
    match config with
    | Some cfg -> config_has_binding cfg ~session_key
    | None -> false
  in
  if from_config then true
  else
    match db with Some db -> db_has_binding ~db ~session_key | None -> false

let principal_id_set () =
  match Sys.getenv_opt "CLAWQ_PRINCIPAL_ID" with
  | Some v when String.trim v <> "" -> true
  | _ -> false

let missing_binding_message ~session_key =
  (* Prefer the post-connector segment as the bind target when present so the
     suggested command matches config-binding room ids (channel id without the
     connector prefix), while still showing the full session key. *)
  let room_hint =
    match String.index_opt session_key ':' with
    | Some i when i + 1 < String.length session_key ->
        String.sub session_key (i + 1) (String.length session_key - i - 1)
    | _ -> session_key
  in
  Printf.sprintf
    "Room-scoped cron session %S has no room profile binding.\n\
     Room memory tools will fail with \"No memory scope or profile binding \
     found for room\".\n\n\
     Fix: bind a profile, then re-add the cron:\n\
    \  clawq rooms bind %s <profile_id>\n\
     Or bind the full session key if that is how the room is registered:\n\
    \  clawq rooms bind %s <profile_id>\n\n\
     Advanced: pass --force to schedule anyway (not recommended)."
    session_key room_hint session_key

let principal_warning ~name =
  Printf.sprintf
    "Warning: job %S looks GitHub-oriented but CLAWQ_PRINCIPAL_ID is unset. \
     The github_account tool will fail until the environment provides a \
     principal id for the daemon/agent process."
    name

(** [validate ~session_key ~name ~message ?config ?db ?force ()] checks room
    prerequisites before scheduling.

    - Non-room sessions: always [Ok].
    - Room sessions without profile binding: [Error] unless [~force:true].
    - GitHub-oriented jobs: soft-warn when [CLAWQ_PRINCIPAL_ID] is unset. *)
let validate ?config ?db ?(force = false) ~session_key ~name ~message () :
    (validation, string) result =
  if not (is_room_scoped_session_key session_key) then Ok empty_validation
  else
    let bound = has_room_profile_binding ?config ?db ~session_key () in
    if (not bound) && not force then
      Error (missing_binding_message ~session_key)
    else
      let warnings = ref [] in
      if (not bound) && force then
        warnings :=
          Printf.sprintf
            "Warning: scheduling room-scoped cron for unbound session %S \
             because --force was set. Room memory tools will fail until you \
             run: clawq rooms bind <room_id> <profile_id>"
            session_key
          :: !warnings;
      if github_oriented ~name ~message && not (principal_id_set ()) then
        warnings := principal_warning ~name :: !warnings;
      Ok { warnings = List.rev !warnings }

let format_result = function
  | Error msg -> Error msg
  | Ok { warnings } ->
      if warnings = [] then Ok None else Ok (Some (String.concat "\n" warnings))
