open Lwt.Syntax
(** Checked integration API. Diagnostics contain fixed classes only. *)

type error =
  | Policy_denied
  | Transport of string
  | Completion_unknown of string

type call =
  string -> (string * Yojson.Safe.t) list -> (Yojson.Safe.t, error) result Lwt.t

let get key = function
  | `Assoc fields -> Option.value (List.assoc_opt key fields) ~default:`Null
  | _ -> `Null

let field key json = match get key json with `String s -> s | _ -> ""

let protect f =
  Lwt.catch f (function
    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
    | _ -> Lwt.return (Error (Transport "transport")))

let checked call method_ params = protect (fun () -> call method_ params)

let create ~call ~title ~markdown =
  let+ result =
    checked call "canvases.create"
      [
        ("title", `String title);
        ( "document_content",
          `Assoc
            [ ("type", `String "markdown"); ("markdown", `String markdown) ] );
      ]
  in
  match result with
  | Error _ as e -> e
  | Ok json ->
      let id = field "canvas_id" json in
      if Slack_config_types.valid_id "F" id then Ok id
      else Error (Transport "invalid_response")

let safe_permalink link =
  try
    let u = Uri.of_string link in
    Uri.scheme u = Some "https"
    && Uri.userinfo u = None
    && Uri.port u = None
    && (match Uri.host u with
      | Some h -> h = "slack.com" || String.ends_with ~suffix:".slack.com" h
      | _ -> false)
    && String.for_all
         (fun c ->
           Char.code c > 32
           && Char.code c <> 127
           && not (List.mem c [ '<'; '>'; '|'; '\\'; '"' ]))
         link
  with _ -> false

let permalink ~call ~canvas_id =
  let+ result = checked call "files.info" [ ("file", `String canvas_id) ] in
  match result with
  | Error _ as e -> e
  | Ok json ->
      let file = get "file" json in
      let link = field "permalink" file in
      if field "id" file = canvas_id && safe_permalink link then Ok link
      else Error (Transport "invalid_response")

let grant_read ~call ~canvas_id ~channel_id =
  let+ result =
    checked call "canvases.access.set"
      [
        ("canvas_id", `String canvas_id);
        ("access_level", `String "read");
        ("channel_ids", `List [ `String channel_id ]);
      ]
  in
  Result.map (fun _ -> ()) result

let delete ~call ~canvas_id =
  let+ result =
    checked call "canvases.delete" [ ("canvas_id", `String canvas_id) ]
  in
  Result.map (fun _ -> ()) result

(* A verified destination is separate from presentation metadata. Cached room
   evidence is valid only for the config under which it was fetched. *)
type destination = {
  session_key : string;
  channel_id : string;
  thread_ts : string option;
  requester : string;
}

type authority = {
  authorize : unit -> (unit, error) result Lwt.t;
  call : call;
  raw_check : uri:string -> (unit, error) result;
}

let methods =
  [
    ("auth.test", "POST");
    ("users.info", "GET");
    ("conversations.info", "GET");
    ("conversations.members", "GET");
    ("canvases.create", "POST");
    ("files.info", "GET");
    ("canvases.access.set", "POST");
    ("canvases.delete", "POST");
    ("chat.postMessage", "POST");
    ("chat.update", "POST");
    ("chat.delete", "POST");
    ("files.getUploadURLExternal", "GET");
    ("files.completeUploadExternal", "POST");
    ("files.delete", "POST");
  ]

let selectors (d : destination) method_ params =
  let keys = List.map fst params in
  let allowed =
    match method_ with
    | "auth.test" -> []
    | "users.info" -> [ "user" ]
    | "conversations.info" -> [ "channel" ]
    | "conversations.members" -> [ "channel"; "limit"; "cursor" ]
    | "canvases.create" -> [ "title"; "document_content" ]
    | "files.info" | "files.delete" -> [ "file" ]
    | "canvases.access.set" -> [ "canvas_id"; "access_level"; "channel_ids" ]
    | "canvases.delete" -> [ "canvas_id" ]
    | "chat.postMessage" ->
        [
          "channel";
          "thread_ts";
          "text";
          "parse";
          "unfurl_links";
          "unfurl_media";
        ]
    | "chat.update" ->
        [ "channel"; "ts"; "text"; "parse"; "unfurl_links"; "unfurl_media" ]
    | "chat.delete" -> [ "channel"; "ts" ]
    | "files.getUploadURLExternal" -> [ "filename"; "length" ]
    | "files.completeUploadExternal" ->
        [ "channel_id"; "thread_ts"; "initial_comment"; "files" ]
    | _ -> []
  in
  let selector =
    match method_ with
    | "chat.postMessage" | "chat.update" | "chat.delete" | "conversations.info"
    | "conversations.members" ->
        Some "channel"
    | "files.completeUploadExternal" -> Some "channel_id"
    | _ -> None
  in
  let parent = match d.thread_ts with None -> `Null | Some s -> `String s in
  let all =
    List.for_all
      (fun (k, v) ->
        match k with
        | "channel" | "channel_id" | "channels" | "conversation"
        | "conversation_id" ->
            Some k = selector && v = `String d.channel_id
        | "thread_ts" ->
            List.mem method_
              [ "chat.postMessage"; "files.completeUploadExternal" ]
            && v = parent
        | "channel_ids" ->
            method_ = "canvases.access.set"
            && v = `List [ `String d.channel_id ]
        | "user_ids" | "users" | "reply_broadcast" -> false
        | _ -> true)
      params
  in
  List.length keys = List.length (List.sort_uniq String.compare keys)
  && List.for_all (fun k -> List.mem k allowed) keys
  && List.mem_assoc method_ methods
  && all
  && (match selector with
    | None -> true
    | Some key -> List.assoc_opt key params = Some (`String d.channel_id))
  && (if List.mem method_ [ "chat.postMessage"; "files.completeUploadExternal" ]
      then
        List.assoc_opt "thread_ts" params
        = Option.map (fun s -> `String s) d.thread_ts
      else true)
  && (method_ <> "canvases.access.set"
     || List.assoc_opt "channel_ids" params
        = Some (`List [ `String d.channel_id ])
        && List.assoc_opt "access_level" params = Some (`String "read"))

let decode status body =
  if status = 429 then Error (Transport "rate_limited")
  else if status < 200 || status >= 300 then Error (Transport "http_status")
  else
    match Yojson.Safe.from_string body with
    | exception _ -> Error (Transport "invalid_response")
    | json when get "ok" json = `Bool true -> Ok json
    | json ->
        let code = field "error" json in
        (* Only fixed codes affect control flow; arbitrary Slack strings are data. *)
        Error
          (Transport
             (if
                List.mem code
                  [
                    "invalid_arguments";
                    "invalid_arg_name";
                    "ratelimited";
                    "internal_error";
                    "file_not_found";
                  ]
              then code
              else "slack_error"))

let make_authority ~manager ?route (d : destination) =
  let verified = ref None
  and snapshot = ref None
  and canvas_eligible = ref false in
  let current () = Session.get_config manager in
  let access_key =
    Access_snapshot.access_session_key ~session_key:d.session_key
      (Some d.channel_id)
  in
  let bound =
    match route with
    | None -> true
    | Some r ->
        Slack_thread_route.valid r && r.channel = d.channel_id
        && d.thread_ts = Some r.root
  in
  let preflight_unchecked ?uri ?(sensitive = false) method_ verb =
    let cfg = current () in
    match cfg.channels.slack with
    | Some sc
      when bound
           && Runtime_config.slack_has_valid_credentials sc
           && Slack_config_types.valid_id "CGD" d.channel_id
           && Slack_config_types.valid_id "UW" d.requester
           && Slack_api.is_allowed ~config:sc ~channel_id:d.channel_id
                ~user_id:d.requester -> (
        let access =
          Runtime_config.resolve_effective_access cfg ~session_key:access_key ()
        in
        let rules =
          Runtime_config.effective_egress_rules cfg access.egress_rules
        in
        let db = Session.get_db manager in
        let snap =
          match (!snapshot, db) with
          | Some (old, s), Some db
            when old == cfg
                 && Access_snapshot.get_by_id ~db s.Access_snapshot.id = Some s
            ->
              Some s
          | _ -> None
        in
        if sensitive && (db = None || snap = None) then Error Policy_denied
        else
          let audit : Policy_http_client.audit_context =
            {
              db;
              session_key = Some access_key;
              snapshot_id =
                Option.map (fun (s : Access_snapshot.t) -> s.id) snap;
              tool_name = Some "slack_final_reply";
              profile_id =
                Option.bind snap (fun s -> s.Access_snapshot.profile_id);
              credential_handle_ids = [];
            }
          in
          match
            Policy_http_client.check_policy ~rules
              ~uri:
                (Option.value uri ~default:("https://slack.com/api/" ^ method_))
              ~method_:verb ~audit ~strict_audit:sensitive
              ~sensitive_uri:sensitive ()
          with
          | Ok () -> Ok sc.bot_token
          | Error _ -> Error Policy_denied)
    | _ -> Error Policy_denied
  in
  let preflight ?uri ?sensitive method_ verb =
    try preflight_unchecked ?uri ?sensitive method_ verb
    with _ -> Error Policy_denied
  in
  let raw method_ params =
    match List.assoc_opt method_ methods with
    | None -> Lwt.return (Error Policy_denied)
    | Some verb -> (
        match preflight method_ verb with
        | Error e -> Lwt.return (Error e)
        | Ok token ->
            protect (fun () ->
                let+ status, body =
                  Slack_standing_order_slack.default_http ~verb ~token ~method_
                    ~params
                in
                decode status body))
  in
  let authorize () =
    let cfg = current () in
    match preflight "conversations.info" "GET" with
    | Error e -> Lwt.return (Error e)
    | Ok _ -> (
        let* permission =
          Slack_thread_api.authorize_request ~manager ~cfg
            ~channel_id:d.channel_id
            ~team:(Option.map (fun (r : Slack_thread_route.t) -> r.team) route)
            ~user:d.requester
            ~read:(fun method_ params ->
              let+ r = raw method_ params in
              Result.map_error (fun _ -> "Slack requester proof denied") r)
        in
        match permission with
        | Error _ -> Lwt.return (Error Policy_denied)
        | Ok proof ->
            let room = proof.Slack_thread_api.conversation in
            let im = get "is_im" room = `Bool true in
            if current () != cfg || d.channel_id.[0] = 'D' <> im then
              Lwt.return (Error Policy_denied)
            else (
              verified := Some cfg;
              canvas_eligible :=
                (not im)
                && get "is_im" room = `Bool false
                && get "is_mpim" room = `Bool false;
              (match Session.get_db manager with
              | Some db -> (
                  try
                    let s =
                      match !snapshot with
                      | Some (previous, s)
                        when previous == cfg
                             && Access_snapshot.get_by_id ~db
                                  s.Access_snapshot.id
                                = Some s ->
                          s
                      | _ ->
                          let s =
                            Access_snapshot.create ~config:cfg
                              ~work_type:Access_snapshot.Room_turn
                              ~session_key:d.session_key ~room_id:d.channel_id
                              ~room_policy_decision:"allow" ()
                          in
                          Access_snapshot.persist ~db s;
                          s
                    in
                    snapshot := Some (cfg, s)
                  with _ -> snapshot := None)
              | None -> ());
              Lwt.return (Ok ())))
  in
  let call method_ params =
    if not (selectors d method_ params) then Lwt.return (Error Policy_denied)
    else
      let* a = authorize () in
      match a with
      | Error e -> Lwt.return (Error e)
      | Ok () ->
          if
            List.mem method_ [ "canvases.create"; "canvases.access.set" ]
            && not !canvas_eligible
          then Lwt.return (Error (Transport "canvas_ineligible"))
          else raw method_ params
  in
  let raw_check ~uri =
    if
      not (match !verified with Some old -> old == current () | None -> false)
    then Error Policy_denied
    else
      Result.map (fun _ -> ()) (preflight ~uri ~sensitive:true "upload" "POST")
  in
  { authorize; call; raw_check }
