open Lwt.Syntax
module C = Slack_canvas
module D = Slack_reply_delivery

let name = "slack_canvas_create"

let outside =
  "slack_canvas_create requires an authorized Slack turn. Reply in the current \
   conversation, or run this tool from Slack; do not supply a channel ID or \
   token."

let dm =
  "slack_canvas_create works in channels; in a DM reply normally and clawq \
   attaches long answers as a file."

let conversation ~channel_id json =
  let room = C.get "channel" json in
  if C.field "id" room <> channel_id then Slack_reply_context.Unknown
  else
    match (C.get "is_im" room, C.get "is_mpim" room) with
    | `Bool true, (`Bool false | `Null) -> Slack_reply_context.Direct
    | `Bool false, `Bool true -> Group_direct
    | `Bool false, `Bool false -> Channel
    | _ -> Unknown

(* Presentation only. An unknown/missing classification never promises a
   channel canvas. This accessor cannot create active-turn tool authority. *)
let with_reply_context ~manager ~key ~channel_id ~requester f =
  let authority =
    C.make_authority ~manager
      ?route:(Slack_thread_route.of_key key)
      {
        C.session_key = key;
        channel_id;
        thread_ts =
          Option.map
            (fun (r : Slack_thread_route.t) -> r.root)
            (Slack_thread_route.of_key key);
        requester;
      }
  in
  let* metadata =
    C.protect (fun () ->
        authority.call "conversations.info" [ ("channel", `String channel_id) ])
  in
  let conversation =
    match metadata with
    | Ok json -> conversation ~channel_id json
    | Error _ -> Slack_reply_context.Unknown
  in
  Slack_reply_context.with_context { connector = "slack"; conversation } f

let with_ingress_reply_context ~session_manager ?thread_run body f =
  match Slack_api.parse_event body with
  | Some (Slack_api.Message { bot_id = None; channel_id; user_id; _ }) ->
      let key =
        match thread_run with
        | Some (r : Slack_thread_runs.run) -> r.key
        | None ->
            Slack_api.resolve_session_key ~session_manager ~channel_id ~user_id
      in
      with_reply_context ~manager:session_manager ~key ~channel_id
        ~requester:user_id f
  | _ -> Lwt.with_value Slack_reply_context.key None f

let arguments = function
  | `Assoc fields
    when List.length fields = 2
         && List.sort String.compare (List.map fst fields)
            = [ "markdown"; "title" ] -> (
      match (List.assoc "title" fields, List.assoc "markdown" fields) with
      | `String raw_title, `String markdown ->
          let title = String.trim raw_title in
          if
            (not (String.is_valid_utf_8 title))
            || title = ""
            || Slack_config_types.code_points title > 120
          then
            Error
              "Error: title must be non-empty valid UTF-8, at most 120 Unicode \
               characters after trimming; supply a short canvas title."
          else if String.trim markdown = "" then
            Error
              "Error: markdown must contain the complete non-empty Markdown \
               write-up."
          else
            Result.map
              (fun markdown -> (title, markdown))
              (Result.map_error
                 (fun _ ->
                   "Error: markdown must be valid UTF-8 and at most 1048576 \
                    bytes/characters; reply normally for automatic file \
                    fallback.")
                 (Slack_long_reply_policy.prepare_markdown markdown))
      | _ -> Error "Error: supply title and markdown as strings.")
  | _ ->
      Error
        "Error: supply only title and markdown; the current Slack turn selects \
         the destination. Do not supply a token, channel or parent."

let make ~current_config =
  let invoke ?(context = Tool.default_context) args =
    match Slack_huddle_context.resolve ~tool_name:name context with
    | Error _ -> Lwt.return outside
    | Ok (turn, snapshot) -> (
        match arguments args with
        | Error message -> Lwt.return message
        | Ok (title, markdown) ->
            let manager = turn.request.manager in
            let access_key =
              Access_snapshot.access_session_key
                ?session_key:snapshot.session_key snapshot.room_id
            in
            let legacy_proof_config = ref None in
            (* The frozen grant AND the live policy must allow every operation.
               Use current credentials, with a strict audit under the tool's
               original snapshot; never derive destinations from arguments. *)
            let preflight method_ verb =
              let runtime : Runtime_config.t = current_config () in
              match Slack_huddle_context.resolve ~tool_name:name context with
              | Error _ -> Error C.Policy_denied
              | Ok _
                when match !legacy_proof_config with
                     | Some cfg -> cfg != runtime
                     | None -> false ->
                  Error C.Policy_denied
              | Ok _
                when Option.is_some
                       (Option.bind context.interrupt_check (fun check ->
                            check ())) ->
                  Error C.Policy_denied
              | Ok _ -> (
                  let access =
                    Runtime_config.resolve_effective_access runtime
                      ~session_key:access_key ()
                  in
                  match runtime.channels.slack with
                  | Some sc
                    when runtime.security.tools_enabled
                         && Runtime_config.slack_has_valid_credentials sc
                         && Slack_api.is_allowed ~config:sc
                              ~channel_id:turn.request.channel_id
                              ~user_id:turn.request.user_id
                         && Runtime_config.room_profile_tool_denial_for_session
                              runtime ~session_key:access_key ~tool_name:name
                              ~equivalence_names:[ name ] ()
                            = None ->
                      let rules =
                        Runtime_config.effective_egress_rules runtime
                          access.egress_rules
                      in
                      let audit : Policy_http_client.audit_context =
                        {
                          db = Session.get_db manager;
                          session_key = context.session_key;
                          snapshot_id = context.snapshot_id;
                          tool_name = Some name;
                          profile_id = context.profile_id;
                          credential_handle_ids = [];
                        }
                      in
                      Result.map
                        (fun () -> sc.bot_token)
                        (Result.map_error
                           (fun _ -> C.Policy_denied)
                           (Policy_http_client.check_policy ~rules
                              ~uri:("https://slack.com/api/" ^ method_)
                              ~method_:verb ~audit ~strict_audit:true ()))
                  | _ -> Error C.Policy_denied)
            in
            let raw_call method_ params =
              match List.assoc_opt method_ C.methods with
              | None -> Lwt.return (Error C.Policy_denied)
              | Some verb -> (
                  match preflight method_ verb with
                  | Error e -> Lwt.return (Error e)
                  | Ok token ->
                      C.protect (fun () ->
                          let+ status, body =
                            Slack_standing_order_slack.default_http ~verb ~token
                              ~method_ ~params
                          in
                          C.decode status body))
            in
            let underlying =
              Option.value
                (Lwt.get Slack_transport.key)
                ~default:Slack_standing_order_slack.api_transport
            in
            let checked_http verb send ~uri =
              let method_ = Filename.basename (Uri.path (Uri.of_string uri)) in
              match preflight method_ verb with
              | Error _ -> Lwt.return (403, "{}")
              | Ok _ -> send ()
            in
            let transport : Slack_transport.t =
              {
                get =
                  (fun ~uri ~headers ->
                    checked_http "GET"
                      (fun () -> underlying.get ~uri ~headers)
                      ~uri);
                post =
                  (fun ~uri ~headers ~body ->
                    checked_http "POST"
                      (fun () -> underlying.post ~uri ~headers ~body)
                      ~uri);
              }
            in
            Slack_transport.with_http transport (fun () ->
                let channel_id = turn.request.channel_id in
                let* metadata =
                  raw_call "conversations.info"
                    [ ("channel", `String channel_id) ]
                in
                let kind =
                  match metadata with
                  | Ok json -> conversation ~channel_id json
                  | _ -> Unknown
                in
                if kind = Direct || kind = Group_direct then Lwt.return dm
                else if kind <> Channel then
                  Lwt.return
                    "Error: Slack channel metadata could not be verified; \
                     reply normally or retry from an authorized Slack channel."
                else
                  let route = Slack_thread_route.of_key turn.request.key in
                  let thread_ts =
                    match route with
                    | Some r -> Some r.root
                    | None -> turn.request.thread_ts
                  in
                  let target : D.target =
                    {
                      session_key = turn.request.key;
                      channel_id;
                      thread_ts;
                      requester = turn.request.user_id;
                      source_ts = Some turn.request.message_id;
                      conversation = Channel;
                    }
                  in
                  let authority =
                    C.make_authority ~manager ?route (D.destination target)
                  in
                  let authorize _ =
                    match preflight "conversations.info" "GET" with
                    | Error e -> Lwt.return (Error e)
                    | Ok _ -> (
                        (* Native routes already check human identity/membership.
                       Legacy ingress needs the same proof via the huddle reader. *)
                        match route with
                        | Some _ -> authority.authorize ()
                        | None -> (
                            let http ~token:_ ~method_ ~params =
                              match preflight method_ "GET" with
                              | Error _ -> Lwt.return (403, "{}")
                              | Ok token ->
                                  Slack_standing_order_slack.default_http
                                    ~verb:"GET" ~token ~method_ ~params
                            in
                            let runtime = current_config () in
                            let* proof =
                              Slack_huddles.authorize
                                ~check:(fun m ->
                                  Result.map_error
                                    (fun _ -> "policy_denied")
                                    (Result.map
                                       (fun _ -> ())
                                       (preflight m "GET")))
                                ~http
                                ~authority:
                                  (Slack_huddles.Human_turn
                                     { turn with config = runtime })
                                ~channel_id
                            in
                            match proof with
                            | Error _ -> Lwt.return (Error C.Policy_denied)
                            | Ok _ when current_config () != runtime ->
                                Lwt.return (Error C.Policy_denied)
                            | Ok _ ->
                                (* Keep the human/room proof tied to its config
                                   through further awaited authority checks and
                                   preflight at the synchronous HTTP boundary. *)
                                legacy_proof_config := Some runtime;
                                authority.authorize ()))
                  in
                  let deps : D.deps =
                    {
                      current_config;
                      authorize;
                      call =
                        (fun _ method_ params ->
                          match
                            preflight method_
                              (Option.value
                                 (List.assoc_opt method_ C.methods)
                                 ~default:"POST")
                          with
                          | Error e -> Lwt.return (Error e)
                          | Ok _ -> authority.call method_ params);
                      upload =
                        (fun _ ~summary:_ ~body:_ ->
                          Lwt.return (Error C.Policy_denied));
                      post_text =
                        (fun ~channel_id:_ ~thread_ts:_ ~text:_ ->
                          Lwt.return (Error C.Policy_denied));
                    }
                  in
                  let* result =
                    D.publish_canvas ~deps ~target ~title ~markdown
                      ~carrier:(Slack_long_reply_policy.escape title)
                  in
                  match result with
                  | Ok artifact ->
                      Lwt.return
                        (Yojson.Safe.to_string
                           (`Assoc
                              [
                                ("canvas_id", `String artifact.canvas_id);
                                ("permalink", `String artifact.permalink);
                                ( "shared_message_ts",
                                  `String artifact.shared_message_ts );
                              ]))
                  | Error _ ->
                      Lwt.return
                        "Error: Slack canvas sharing failed or access was \
                         revoked; owned artifacts were cleaned up where \
                         permitted. Check app scopes, channel policy and \
                         egress, then reply normally so automatic fallback can \
                         preserve the answer."))
  in
  let checked_invoke = invoke in
  let invoke ?context args =
    Lwt.catch
      (fun () -> checked_invoke ?context args)
      (function
        | Lwt.Canceled -> Lwt.fail Lwt.Canceled
        | _ ->
            Lwt.return
              "Error: Slack canvas publication could not be verified; check \
               app permissions and reply normally for automatic fallback.")
  in
  { (Tools_builtin.slack_canvas_create ()) with invoke }

let register ~current_config registry =
  Tool_registry.replace registry (make ~current_config)
