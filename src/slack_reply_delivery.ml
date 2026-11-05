open Lwt.Syntax
(** Final-only presentation with call-local artifact ownership. No recovery
    state: replay after a crash may produce a duplicate presentation (R1). *)

module C = Slack_canvas
module U = Slack_file_upload

type upload_outcome = Shared | Definitely_not_shared | Unresolved of string

type target = {
  session_key : string;
  channel_id : string;
  thread_ts : string option;
  requester : string;
  source_ts : string option;
  conversation : Slack_reply_context.conversation;
}

type deps = {
  current_config : unit -> Runtime_config.t;
  authorize : target -> (unit, C.error) result Lwt.t;
  call : target -> C.call;
  upload :
    target ->
    summary:string ->
    body:string ->
    (upload_outcome, C.error) result Lwt.t;
  post_text : Slack_final_text.send;
}

type artifact = {
  canvas_id : string;
  permalink : string;
  shared_message_ts : string;
}

type outcome = Canvas_sent of artifact | File_sent | Text_sent | Suppressed

let ( let*? ) p f =
  let* r = p in
  match r with Ok v -> f v | Error e -> Lwt.return (Error e)

let thread_params t =
  match t.thread_ts with None -> [] | Some ts -> [ ("thread_ts", `String ts) ]

let shared ~target ~file_id file =
  C.field "id" file = file_id
  && List.exists
       (fun visibility ->
         match
           C.get target.channel_id (C.get visibility (C.get "shares" file))
         with
         | `List records ->
             List.exists
               (fun record ->
                 Slack_huddles.valid_ts (C.field "ts" record)
                 &&
                 match (target.thread_ts, C.get "thread_ts" record) with
                 | None, (`Null | `String "") -> true
                 | Some p, `String actual -> p = actual
                 | _ -> false)
               records
         | _ -> false)
       [ "public"; "private" ]

let error_of_upload = function
  | U.Remote e -> e
  | U.Invalid_upload_url -> C.Policy_denied
  | U.Invalid_response -> C.Transport "invalid_response"
  | U.Http_status _ -> C.Transport "http_status"
  | U.Transport -> C.Transport "transport"

let upload_file ~call ~post ~clock ~wait ~target ~summary ~body =
  let* allocated =
    U.allocate ~call ~filename:"clawq-answer.md" ~length:(String.length body)
  in
  match allocated with
  | Error (U.Remote C.Policy_denied) -> Lwt.return (Error C.Policy_denied)
  | Error _ -> Lwt.return (Ok Definitely_not_shared)
  | Ok ticket -> (
      let* posted = C.protect (fun () -> post ~uri:ticket.upload_url ~body) in
      match posted with
      | Error C.Policy_denied -> Lwt.return (Error C.Policy_denied)
      | Error _ -> Lwt.return (Ok Definitely_not_shared)
      | Ok () -> (
          let* completed =
            U.complete ~call ~channel_id:target.channel_id
              ~thread_ts:target.thread_ts ~initial_comment:summary
              ~file_ids:[ ticket.file_id ]
          in
          match completed with
          | Error (U.Remote C.Policy_denied) ->
              Lwt.return (Error C.Policy_denied)
          | Error
              (U.Remote (C.Transport ("invalid_arguments" | "invalid_arg_name")))
            ->
              Lwt.return (Ok Definitely_not_shared)
          | _ ->
              let has_evidence json =
                shared ~target ~file_id:ticket.file_id (C.get "file" json)
                ||
                match C.get "files" json with
                | `List fs ->
                    List.exists (shared ~target ~file_id:ticket.file_id) fs
                | _ -> false
              in
              if match completed with Ok j -> has_evidence j | _ -> false then
                Lwt.return (Ok Shared)
              else
                let deadline = clock () +. 10. in
                let rec reconcile remaining =
                  if remaining = 0 || clock () >= deadline then
                    Lwt.return (Ok (Unresolved ticket.file_id))
                  else
                    let pending =
                      C.checked call "files.info"
                        [ ("file", `String ticket.file_id) ]
                    in
                    let* result =
                      if not (Lwt.is_sleeping pending) then pending
                      else
                        Lwt.pick
                          [
                            pending;
                            (let* () = wait (max 0. (deadline -. clock ())) in
                             Lwt.return (Error (C.Transport "deadline")));
                          ]
                    in
                    match result with
                    | Error C.Policy_denied ->
                        Lwt.return (Error C.Policy_denied)
                    | Ok j when clock () <= deadline && has_evidence j ->
                        Lwt.return (Ok Shared)
                    | _ ->
                        if remaining = 1 || clock () >= deadline then
                          Lwt.return (Ok (Unresolved ticket.file_id))
                        else
                          let* () =
                            wait (min 1. (max 0. (deadline -. clock ())))
                          in
                          reconcile (remaining - 1)
                in
                reconcile 3))

let destination t : C.destination =
  {
    session_key = t.session_key;
    channel_id = t.channel_id;
    thread_ts = t.thread_ts;
    requester = t.requester;
  }

let make_deps ~manager ~post_text =
  let cached = ref None in
  let authority t =
    match !cached with
    | Some (original, a) when original = t -> a
    | _ ->
        let a =
          C.make_authority ~manager
            ?route:(Slack_thread_route.of_key t.session_key)
            (destination t)
        in
        cached := Some (t, a);
        a
  in
  let upload t ~summary ~body =
    let a = authority t in
    let post ~uri ~body =
      let*? () = a.authorize () in
      let http =
        match Lwt.get Slack_transport.key with
        | None -> U.default_http
        | Some transport ->
            fun (request : U.request) ->
              let+ status, body =
                transport.post ~uri:request.uri ~headers:request.headers
                  ~body:request.body
              in
              { U.status; headers = []; body }
      in
      let+ result =
        U.post ~http
          ~limit:(fun () -> Lwt.return_unit)
          ~authorize:a.raw_check
          ~before_send:(fun () -> a.raw_check ~uri)
          ~uri ~body ()
      in
      Result.map_error error_of_upload result
    in
    upload_file ~call:a.call ~post ~clock:Unix.gettimeofday ~wait:Lwt_unix.sleep
      ~target:t ~summary ~body
  in
  {
    current_config = (fun () -> Session.get_config manager);
    authorize = (fun t -> (authority t).authorize ());
    call = (fun t -> (authority t).call);
    upload;
    post_text;
  }

let checked deps target method_ params =
  let*? () = C.protect (fun () -> deps.authorize target) in
  C.checked (deps.call target) method_ params

(* Only lookups and read grants are safe to retry. Create and completion are
   never repeated, including after a timeout or an invalid response. *)
let retry f =
  let rec loop n =
    let* r = f () in
    match r with
    | Error
        (C.Transport
           ( "rate_limited" | "ratelimited" | "transport" | "http_status"
           | "internal_error" ))
      when n > 0 ->
        loop (n - 1)
    | _ -> Lwt.return r
  in
  loop 2

let publish_canvas ~deps ~target ~title ~markdown ~carrier =
  let carrier = if String.trim carrier = "" then "Full answer" else carrier in
  let call = checked deps target in
  if target.conversation <> Slack_reply_context.Channel then
    Lwt.return (Error C.Policy_denied)
  else
    let*? canvas_id = C.create ~call ~title ~markdown in
    let message = ref None in
    let cleanup original =
      let* deleted = C.delete ~call ~canvas_id in
      let* cleaned =
        match !message with
        | None -> Lwt.return (Ok ())
        | Some ts -> (
            let* edited =
              call "chat.update"
                [
                  ("channel", `String target.channel_id);
                  ("ts", `String ts);
                  ("text", `String carrier);
                  ("parse", `String "none");
                  ("unfurl_links", `Bool false);
                  ("unfurl_media", `Bool false);
                ]
            in
            match edited with
            | Ok _ -> Lwt.return (Ok ())
            | Error C.Policy_denied -> Lwt.return (Error C.Policy_denied)
            | Error _ ->
                let+ r =
                  call "chat.delete"
                    [
                      ("channel", `String target.channel_id); ("ts", `String ts);
                    ]
                in
                Result.map (fun _ -> ()) r)
      in
      Lwt.return
        (match (original, deleted, cleaned) with
        | C.Policy_denied, _, _
        | _, Error C.Policy_denied, _
        | _, _, Error C.Policy_denied ->
            Error C.Policy_denied
        | _ -> Error original)
    in
    let* result =
      let*? permalink = retry (fun () -> C.permalink ~call ~canvas_id) in
      let text = carrier ^ "\n<" ^ permalink ^ "|Read full answer>" in
      let*? posted =
        call "chat.postMessage"
          ([
             ("channel", `String target.channel_id);
             ("text", `String text);
             ("parse", `String "none");
             ("unfurl_links", `Bool false);
             ("unfurl_media", `Bool false);
           ]
          @ thread_params target)
      in
      let ts = C.field "ts" posted in
      if not (Slack_huddles.valid_ts ts) then
        Lwt.return (Error (C.Transport "invalid_response"))
      else (
        message := Some ts;
        let*? () =
          retry (fun () ->
              C.grant_read ~call ~canvas_id ~channel_id:target.channel_id)
        in
        Lwt.return (Ok { canvas_id; permalink; shared_message_ts = ts }))
    in
    match result with Ok _ -> Lwt.return result | Error e -> cleanup e

let deliver ~deps ~target ~body =
  if Slack_api.blank_text body then Lwt.return (Ok Suppressed)
  else
    let cfg = deps.current_config () in
    let policy =
      match cfg.channels.slack with
      | Some s -> s.long_reply
      | None -> Slack_config_types.default_long_reply
    in
    let*? () = C.protect (fun () -> deps.authorize target) in
    let text () =
      let*? () = C.protect (fun () -> deps.authorize target) in
      let+ r =
        C.protect (fun () ->
            deps.post_text ~channel_id:target.channel_id
              ~thread_ts:target.thread_ts ~text:body)
      in
      Result.map (fun () -> Text_sent) r
    in
    let file () =
      let*? () = C.protect (fun () -> deps.authorize target) in
      let* r =
        C.protect (fun () ->
            deps.upload target
              ~summary:(Slack_long_reply_policy.summary body)
              ~body)
      in
      match r with
      | Error e -> Lwt.return (Error e)
      | Ok Shared -> Lwt.return (Ok File_sent)
      | Ok Definitely_not_shared -> text ()
      | Ok (Unresolved id) -> (
          let* deleted =
            checked deps target "files.delete" [ ("file", `String id) ]
          in
          match deleted with
          | Ok _ -> text ()
          | Error _ ->
              Lwt.return
                (Error (C.Completion_unknown "file_completion_unknown")))
    in
    if not (Slack_long_reply_policy.overflows policy body) then text ()
    else if
      policy.mode = Slack_config_types.Canvas
      && target.conversation = Slack_reply_context.Channel
    then
      match Slack_long_reply_policy.prepare_markdown body with
      | Error _ -> file ()
      | Ok markdown -> (
          let* r =
            publish_canvas ~deps ~target
              ~title:
                (Slack_long_reply_policy.automatic_title
                   ~source_ts:target.source_ts)
              ~markdown
              ~carrier:(Slack_long_reply_policy.summary body)
          in
          match r with
          | Ok a -> Lwt.return (Ok (Canvas_sent a))
          | Error C.Policy_denied -> Lwt.return (Error C.Policy_denied)
          | Error _ -> file ())
    else file ()
