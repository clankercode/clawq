open Lwt.Syntax
(** Supported Slack Web API reads only. Never fetch a returned download URL. *)

let obj fields = `Assoc fields
let str s = `String s

let get key = function
  | `Assoc xs -> Option.value (List.assoc_opt key xs) ~default:`Null
  | _ -> `Null

let text = function `String s -> s | _ -> ""
let list = function `List xs -> xs | _ -> []
let field key json = text (get key json)
let flag key json = get key json = `Bool true
let fail message = Lwt.return (Error message)

let ( let*? ) promise f =
  let* result = promise in
  match result with Ok value -> f value | Error _ as error -> Lwt.return error

let valid_id = Slack_config_types.valid_id
let slackbot_user_id = "USLACKBOT"

let valid_ts s =
  String.length s > 0
  && String.length s < 30
  && String.for_all (function '0' .. '9' | '.' -> true | _ -> false) s
  &&
  match float_of_string_opt s with
  | Some n -> Float.is_finite n && n >= 0.
  | None -> false

let http ~token ~method_ ~params =
  let query = List.map (fun (k, v) -> (k, [ text v ])) params in
  let uri =
    Uri.make ~scheme:"https" ~host:"slack.com" ~path:("/api/" ^ method_) ~query
      ()
    |> Uri.to_string
  in
  Http_client.get ~uri ~headers:[ ("Authorization", "Bearer " ^ token) ]

let api ~check ~http ~token method_ params =
  match check method_ with
  | Error reason -> fail reason
  | Ok () ->
      Lwt.catch
        (fun () ->
          let* status, body = http ~token ~method_ ~params in
          if status = 429 then
            fail
              (method_
             ^ ": rate_limited; wait before requesting this page again (no \
                automatic retry).")
          else if status <> 200 then
            fail
              (method_
             ^ ": HTTP failure; check Slack availability and credentials.")
          else
            let json = Yojson.Safe.from_string body in
            if flag "ok" json then Lwt.return (Ok json)
            else
              let repair =
                match field "error" json with
                | "missing_scope" ->
                    "missing_scope; grant the relevant channels/groups:read, \
                     channels/groups:history, users:read or files:read scopes \
                     and reinstall the app."
                | "ratelimited" | "rate_limited" ->
                    "rate_limited; wait before requesting this page again (no \
                     automatic retry)."
                | "not_in_channel" | "channel_not_found" | "file_not_found"
                | "no_permission" | "access_denied" ->
                    "content unavailable or access denied; confirm channel \
                     membership and file sharing."
                | "invalid_auth" | "token_revoked" | "account_inactive"
                | "not_authed" ->
                    "authentication failed; update the bot credential."
                | "invalid_cursor" ->
                    "invalid_cursor; restart discovery without a cursor."
                | _ ->
                    "Slack rejected the read; verify app permissions and \
                     source availability."
              in
              fail (method_ ^ ": " ^ repair))
        (function
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | _ ->
              fail
                (method_
               ^ ": request failed; check Slack connectivity and retry later."))

let allowed config channel user =
  Slack_api.is_allowed ~config ~channel_id:channel ~user_id:user

type grant_fire = {
  order_id : string;
  fire_id : string option;
  team_id : string;
  channel_id : string;
  grantor_user_id : string;
  config : Runtime_config.t;
}
(** A standing order's authority for one fire: the grantor's identity, pinned to
    the order's team and channel. Never an admin, never cross-channel. *)

type authority =
  | Human_turn of Slack_huddle_context.turn
  | Grant_fire of grant_fire

let share_flags =
  [ "is_ext_shared"; "is_shared"; "is_org_shared"; "is_pending_ext_shared" ]

let authorize ~check ~http ~authority ~channel_id =
  let config, origin, user, grant =
    match authority with
    | Human_turn (turn : Slack_huddle_context.turn) ->
        (turn.config, turn.request.channel_id, turn.request.user_id, None)
    | Grant_fire g -> (g.config, g.channel_id, g.grantor_user_id, Some g)
  in
  let is_grant = Option.is_some grant in
  (* The turn's admin status was frozen at ingress and must still hold; a grant
     is never an admin, so it can take no admin override or cross-channel read. *)
  let live_admin () =
    match authority with
    | Human_turn turn -> (
        match Session.get_db turn.request.manager with
        | Some db ->
            turn.is_admin && Admin.is_admin ~db ~channel:"slack" ~sender_id:user
        | None -> false)
    | Grant_fire _ -> false
  in
  match config.channels.slack with
  | None ->
      fail
        "Slack is disabled; configure the connector before using huddle tools."
  | Some slack when slack.bot_token = "" ->
      fail
        "Slack bot credential is missing; configure channels.slack.bot_token."
  | Some slack ->
      if is_grant && channel_id <> origin then
        fail
          "Grant authority is pinned to the order's channel; no content was \
           read."
      else if
        not
          (valid_id "CG" channel_id && valid_id "CG" origin
         && valid_id "UW" user)
      then
        fail
          "Huddle tools require explicit Slack channel IDs (C/G) and a \
           verified user. DMs are not supported."
      else if not (allowed slack channel_id user && allowed slack origin user)
      then
        fail
          "Slack user/channel allowlist denies this read; ask an operator to \
           review the connector policy."
      else
        let call = api ~check ~http ~token:slack.bot_token in
        let*? auth = call "auth.test" [] in
        let team = field "team_id" auth in
        let*? () =
          match grant with
          | Some g when team <> g.team_id ->
              fail
                "Grant authority belongs to another Slack installation (team \
                 mismatch); no content was read."
          | _ -> Lwt.return (Ok ())
        in
        let*? human = call "users.info" [ ("user", str user) ] in
        let human = get "user" human in
        let bot = field "user_id" auth in
        if
          team = ""
          || (not (valid_id "UW" bot))
          || field "id" human <> user
          || field "team_id" human <> team
          || flag "deleted" human || flag "is_bot" human
          || get "deleted" human = `Null
          || get "is_bot" human = `Null
          || get "is_restricted" human = `Null
          || get "is_ultra_restricted" human = `Null
        then
          fail
            "Slack requester identity is unavailable or outside this \
             installation; no content was read."
        else
          let guest =
            flag "is_restricted" human || flag "is_ultra_restricted" human
          in
          let is_admin = live_admin () in
          (* Even admins must be members. Cross-channel export into a guest's
           origin is refused; membership never follows from bot visibility. *)
          if channel_id <> origin && (guest || not is_admin) then
            fail
              "Cross-channel huddle reads require a Clawq admin who belongs to \
               both channels; guests must use the source channel."
          else
            let check_channel id =
              let*? response =
                call "conversations.info" [ ("channel", str id) ]
              in
              let channel = get "channel" response in
              if
                field "id" channel <> id
                || get "is_private" channel = `Null
                || not (flag "is_member" channel)
              then
                fail
                  "Slack channel metadata or bot membership could not be \
                   verified; invite the bot and try again."
              else if
                not
                  (Slack_api.check_private_channel_policy ~config:slack
                     ~channel_id:id
                     ~is_private_opt:(Some (flag "is_private" channel)))
              then
                fail
                  "Private-channel policy denies this read; explicitly opt \
                   this channel in before retrying."
              else
                let classification : Runtime_config_types.room_classification =
                  {
                    connector = "slack";
                    room_id = id;
                    scope =
                      (if guest then Rm_external
                       else if
                         flag "is_ext_shared" channel
                         || flag "is_shared" channel
                       then Rm_shared
                       else Rm_group);
                    has_external_users = guest || flag "is_ext_shared" channel;
                    tenant_id = Some team;
                  }
                in
                match
                  Room_policy.evaluate config.external_room_policy
                    ~classification ~is_admin ()
                with
                | Room_policy.Denied reason -> fail reason
                (* A grant needs typed [false] flags: a guest grantor or any
                   sharing, even unreported, is a hard deny whatever the room
                   policy says. *)
                | _
                  when is_grant
                       && (get "is_restricted" human <> `Bool false
                          || get "is_ultra_restricted" human <> `Bool false) ->
                    fail
                      "Grant authority needs a full (non-guest) member as \
                       grantor; no content was read."
                | _
                  when is_grant
                       && List.exists
                            (fun key -> get key channel <> `Bool false)
                            share_flags ->
                    fail
                      "Grant authority refuses shared channels (or channels \
                       whose sharing state Slack did not report); no content \
                       was read."
                | Room_policy.Proceed | Room_policy.Proceed_with_warning _
                | Room_policy.Denied_admin_override _ ->
                    let rec members cursor pages =
                      let params =
                        [ ("channel", str id); ("limit", str "200") ]
                        @ if cursor = "" then [] else [ ("cursor", str cursor) ]
                      in
                      let*? result = call "conversations.members" params in
                      let found =
                        List.exists
                          (fun v -> text v = user)
                          (list (get "members" result))
                      in
                      if found then Lwt.return (Ok ())
                      else
                        let next =
                          field "next_cursor" (get "response_metadata" result)
                        in
                        if next <> "" && next <> cursor && pages < 5 then
                          members next (pages + 1)
                        else
                          fail
                            "Requester membership could not be established \
                             within five pages; no channel content was read."
                    in
                    members "" 1
            in
            let*? () = check_channel origin in
            let*? () =
              if channel_id = origin then Lwt.return (Ok ())
              else check_channel channel_id
            in
            Lwt.return (Ok call)

let pagination json =
  let cursor = field "next_cursor" (get "response_metadata" json) in
  let exhausted = cursor = "" && not (flag "has_more" json) in
  [
    ("next_cursor", if cursor = "" then `Null else str cursor);
    ("pagination_exhausted", `Bool exhausted);
    ("complete", `Bool (exhausted && not (flag "is_limited" json)));
  ]

let huddle json =
  let room = get "room" json in
  field "subtype" json = "huddle_thread"
  && valid_id "R" (field "id" room)
  && valid_ts (field "ts" json)

let provenance ~channel_id source =
  [
    ("channel_id", str channel_id);
    ("huddle_ts", get "ts" source);
    ("huddle_id", get "id" (get "room" source));
    ("source", str "conversations.history:huddle_thread.room");
  ]

let project_huddle ~channel_id source =
  let room = get "room" source in
  obj
    (provenance ~channel_id source
    @ List.map
        (fun key -> (key, get key room))
        [
          "date_start";
          "date_end";
          "thread_root_ts";
          "canvas_thread_ts";
          "attached_file_ids";
        ])

let discover ~call ~channel_id ~oldest ~latest ~cursor ~limit =
  let*? result =
    call "conversations.history"
      ([
         ("channel", str channel_id);
         ("oldest", str oldest);
         ("latest", str latest);
         ("inclusive", str "true");
         ("limit", str (string_of_int limit));
       ]
      @ if cursor = "" then [] else [ ("cursor", str cursor) ])
  in
  let*? messages =
    match get "messages" result with
    | `List messages -> Lwt.return (Ok messages)
    | _ ->
        fail
          "conversations.history: malformed message page; no completeness \
           claim is possible."
  in
  let in_window m =
    match float_of_string_opt (field "ts" m) with
    | Some n -> n >= float_of_string oldest && n <= float_of_string latest
    | None -> false
  in
  let huddles =
    messages
    |> List.filter (fun m -> huddle m && in_window m)
    |> List.map (project_huddle ~channel_id)
  in
  Lwt.return
    (Ok
       (obj
          ([
             ("huddles", `List huddles);
             ("is_limited", `Bool (flag "is_limited" result));
             ( "history_coverage",
               str
                 (if flag "is_limited" result then "limited"
                  else if
                    get "pagination_exhausted" (obj (pagination result))
                    = `Bool true
                  then "available_history_exhausted"
                  else "pagination_incomplete") );
             ( "coverage_warning",
               if flag "is_limited" result then
                 str
                   "Slack's workspace history limit hides earlier messages; \
                    pagination cannot retrieve hidden history. An empty result \
                    is not an exhaustive negative search."
               else `Null );
             ("oldest", str oldest);
             ("latest", str latest);
             ("untrusted_content", `Bool true);
             ( "transcript_availability",
               str
                 "conditional; discovery does not establish transcript \
                  availability" );
           ]
          @ pagination result)))

let content ~call ~channel_id ~huddle_ts ~cursor ~thread =
  (* Re-read the exact source on every page: an ID/cursor is not a capability. *)
  let*? history =
    call "conversations.history"
      [
        ("channel", str channel_id);
        ("oldest", str huddle_ts);
        ("latest", str huddle_ts);
        ("inclusive", str "true");
        ("limit", str "1");
      ]
  in
  let*? source_messages =
    match get "messages" history with
    | `List messages -> Lwt.return (Ok messages)
    | _ ->
        fail
          "conversations.history: malformed source page; no file access \
           granted."
  in
  match
    List.find_opt
      (fun m -> huddle m && field "ts" m = huddle_ts)
      source_messages
  with
  | None ->
      fail
        "No accessible huddle_thread exists at huddle_ts; discover it with \
         slack_huddles first."
  | Some source ->
      let room = get "room" source in
      let thread_ts =
        if thread = "canvas" then field "canvas_thread_ts" room
        else match field "thread_root_ts" room with "" -> huddle_ts | ts -> ts
      in
      if not (valid_ts thread_ts) then
        fail
          "The authorized huddle has no valid selected thread reference; \
           select huddle or use returned file references."
      else
        let*? replies =
          call "conversations.replies"
            ([
               ("channel", str channel_id);
               ("ts", str thread_ts);
               ("limit", str "15");
             ]
            @ if cursor = "" then [] else [ ("cursor", str cursor) ])
        in
        let*? messages =
          match get "messages" replies with
          | `List messages -> Lwt.return (Ok messages)
          | _ ->
              fail
                "conversations.replies: malformed thread page; retry the \
                 authorized source later."
        in
        let notes =
          messages
          |> List.filter (fun m ->
              field "ts" m <> huddle_ts
              && field "ts" m <> thread_ts
              && field "text" m <> "")
          |> List.map (fun m ->
              obj
                [
                  ("ts", get "ts" m);
                  ("user", get "user" m);
                  ("text", get "text" m);
                  ( "source",
                    str "conversations.replies (discussion, not transcript)" );
                ])
        in
        let ids =
          List.map text (list (get "attached_file_ids" room))
          @ List.concat_map
              (fun m -> List.map (field "id") (list (get "files" m)))
              (source :: messages)
          |> List.filter (valid_id "F")
          |> List.sort_uniq String.compare
        in
        let rec hydrate seen remaining files transcripts errors = function
          | [] ->
              Lwt.return
                (List.rev files, List.rev transcripts, List.rev errors, false)
          | id :: rest when List.mem id seen ->
              hydrate seen remaining files transcripts errors rest
          | _ when remaining = 0 ->
              Lwt.return
                (List.rev files, List.rev transcripts, List.rev errors, true)
          | id :: rest -> (
              let* result = call "files.info" [ ("file", str id) ] in
              match result with
              | Error reason ->
                  let errors =
                    obj [ ("file_id", str id); ("error", str reason) ] :: errors
                  in
                  let stop =
                    List.exists
                      (fun prefix -> String.starts_with ~prefix reason)
                      [
                        "files.info: rate_limited;";
                        "files.info: missing_scope;";
                        "files.info: authentication failed;";
                      ]
                  in
                  if stop then
                    Lwt.return
                      ( List.rev files,
                        List.rev transcripts,
                        List.rev errors,
                        rest <> [] )
                  else
                    hydrate (id :: seen) (remaining - 1) files transcripts
                      errors rest
              | Ok response ->
                  let file = get "file" response in
                  let origin =
                    field "originating_huddle_id" (get "canvas_metadata" file)
                  in
                  let transcript = get "huddle_transcription" file in
                  let transcript_channel = field "channel_id" transcript in
                  if
                    field "id" file <> id
                    || (origin <> "" && origin <> field "id" room)
                    || transcript_channel <> ""
                       && transcript_channel <> channel_id
                  then
                    hydrate (id :: seen) (remaining - 1) files transcripts
                      (obj
                         [
                           ("file_id", str id);
                           ( "error",
                             str
                               "File provenance does not match the authorized \
                                huddle; content withheld." );
                         ]
                      :: errors)
                      rest
                  else
                    let rec has_text = function
                      | `String s -> String.trim s <> ""
                      | `List xs -> List.exists has_text xs
                      | `Assoc xs ->
                          List.exists
                            (fun (k, v) ->
                              List.mem k
                                [
                                  "text";
                                  "contents";
                                  "elements";
                                  "lines";
                                  "blocks";
                                ]
                              && has_text v)
                            xs
                      | _ -> false
                    in
                    let inline =
                      has_text (get "lines" transcript)
                      || has_text (get "blocks" transcript)
                    in
                    let transcripts =
                      if inline then
                        obj
                          [
                            ("file_id", str id);
                            ( "source",
                              str "files.info:file.huddle_transcription" );
                            ("content", transcript);
                          ]
                        :: transcripts
                      else transcripts
                    in
                    let projection =
                      obj
                        [
                          ("file_id", str id);
                          ( "source",
                            str
                              "files.info (authorized huddle/thread reference)"
                          );
                          ("title", get "title" file);
                          ("preview", get "preview_plain_text" file);
                          ("preview_kind", str "file preview, not transcript");
                          ("huddle_summary_id", get "huddle_summary_id" file);
                          ( "huddle_transcript_file_id",
                            get "huddle_transcript_file_id" file );
                          ("inline_transcript_returned", `Bool inline);
                        ]
                    in
                    let follow = field "huddle_transcript_file_id" file in
                    let rest =
                      if valid_id "F" follow then rest @ [ follow ] else rest
                    in
                    hydrate (id :: seen) (remaining - 1) (projection :: files)
                      transcripts errors rest)
        in
        let* files, transcripts, errors, truncated =
          hydrate [] 8 [] [] [] ids
        in
        let page_complete =
          get "complete" (obj (pagination replies)) = `Bool true
        in
        let available = transcripts <> [] in
        Lwt.return
          (Ok
             (obj
                (provenance ~channel_id source
                @ pagination replies
                @ [
                    ("thread_ts", str thread_ts);
                    ("thread", str thread);
                    ("canvas_thread_ts", get "canvas_thread_ts" room);
                    ("thread_notes", `List notes);
                    ("files", `List files);
                    ("transcripts", `List transcripts);
                    ("file_errors", `List errors);
                    ("files_truncated", `Bool truncated);
                    ( "content_complete",
                      `Bool (page_complete && (not truncated) && errors = []) );
                    ( "transcript_status",
                      str
                        (if available then "inline_content_returned"
                         else if errors <> [] || truncated || not page_complete
                         then "not_returned_in_this_partial_result"
                         else "unavailable_in_returned_data") );
                    ( "unavailability_reason",
                      if available then `Null
                      else
                        str
                          "Slack returned no inline huddle_transcription on \
                           this page. Notes may be disabled, deleted, \
                           restricted, or unavailable through this API; thread \
                           notes/previews are not transcripts." );
                    ( "transcript_completeness",
                      str
                        "not asserted; only returned inline blocks/lines and \
                         time ranges" );
                    ( "completeness_scope",
                      str
                        "source page and file hydration, not entire huddle \
                         transcription" );
                    ("untrusted_content", `Bool true);
                  ])))
