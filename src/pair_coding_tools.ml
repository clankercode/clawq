(* Single pair_coding tool with action parameter for pair coding sessions. *)

type pair_context = {
  db : Sqlite3.db;
  pair_id : string;
  role : Pair_coding_types.role;
  wake_conditions : (Pair_coding_types.role * unit Lwt_condition.t) list;
  session_mgr : Session_core.t;
}

let wake_role ctx (target_role : Pair_coding_types.role) =
  match List.assoc_opt target_role ctx.wake_conditions with
  | Some cond -> Lwt_condition.signal cond ()
  | None -> ()

let enqueue_to_role ctx ~(target_role : Pair_coding_types.role) ~message =
  let target_key =
    Printf.sprintf "pair:%s:%s" ctx.pair_id
      (Pair_coding_types.role_key_suffix target_role)
  in
  let qm : Session_core.queued_message =
    {
      message;
      content_parts = [];
      attachments = [];
      channel_name = Some "pair";
      channel_type = Some "pair";
      sender_id = Some (Pair_coding_types.role_to_string ctx.role);
      sender_name = Some (Pair_coding_types.role_to_string ctx.role);
      user_group = None;
      channel = Some "pair";
      channel_id = Some ctx.pair_id;
      message_id = None;
      inbound_queue_id = None;
      bang = false;
      deferred_followup = false;
    }
  in
  let open Lwt.Syntax in
  let* _queued =
    Session_core.enqueue_message_if_busy ctx.session_mgr ~key:target_key qm
  in
  wake_role ctx target_role;
  Lwt.return_unit

let format_reply_footer ~sender =
  Printf.sprintf
    "\n\
     [Reply with pair_coding(action=\"send_msg\", to=\"%s\", message=\"...\")]"
    (Pair_coding_types.role_to_string sender)

let action_send_msg ctx args =
  let open Yojson.Safe.Util in
  let to_str =
    try args |> member "to" |> to_string
    with _ ->
      failwith
        "Error: 'to' parameter is required for send_msg. Specify target role: \
         \"coder\", \"observer\", or \"coordinator\"."
  in
  let message =
    try args |> member "message" |> to_string
    with _ ->
      failwith
        "Error: 'message' parameter is required for send_msg. Provide the \
         message text to send."
  in
  match Pair_coding_types.role_of_string to_str with
  | None ->
      Lwt.return
        (Printf.sprintf
           "Error: invalid target role '%s'. Must be one of: \"coder\", \
            \"observer\", \"coordinator\"."
           to_str)
  | Some target_role ->
      let formatted =
        Printf.sprintf "[Message from %s]: %s%s"
          (Pair_coding_types.role_to_string ctx.role)
          message
          (format_reply_footer ~sender:ctx.role)
      in
      let open Lwt.Syntax in
      let* () = enqueue_to_role ctx ~target_role ~message:formatted in
      Lwt.return
        (Printf.sprintf "Message sent to %s."
           (Pair_coding_types.role_to_string target_role))

let action_write_note ctx args =
  match ctx.role with
  | Pair_coding_types.Observer -> (
      let session = Pair_coding_state.load_session ~db:ctx.db ~id:ctx.pair_id in
      match session with
      | None -> Lwt.return "Error: pair session not found."
      | Some s -> (
          match s.phase with
          | Pair_coding_types.Coding | Iteration ->
              let open Yojson.Safe.Util in
              let description =
                try args |> member "description" |> to_string
                with _ ->
                  failwith
                    "Error: 'description' parameter is required for \
                     write_note. Describe what you observed."
              in
              let severity_str =
                try args |> member "severity" |> to_string with _ -> "medium"
              in
              let severity =
                match Pair_coding_types.severity_of_string severity_str with
                | Some s -> s
                | None -> Pair_coding_types.Medium
              in
              let category_str =
                try Some (args |> member "category" |> to_string)
                with _ -> None
              in
              let category =
                Option.bind category_str Pair_coding_types.category_of_string
              in
              let file =
                try Some (args |> member "file" |> to_string) with _ -> None
              in
              let line =
                try Some (args |> member "line" |> to_int) with _ -> None
              in
              let note_id =
                Pair_coding_state.add_note ~db:ctx.db ~session_id:ctx.pair_id
                  ~description ?category ~severity ?file ?line ()
              in
              let notification =
                Printf.sprintf "[Observer note #%d (%s)]: %s%s%s" note_id
                  (Pair_coding_types.severity_to_string severity)
                  description
                  (match file with
                  | Some f ->
                      Printf.sprintf " (file: %s%s)" f
                        (match line with
                        | Some l -> Printf.sprintf ":%d" l
                        | None -> "")
                  | None -> "")
                  (format_reply_footer ~sender:ctx.role)
              in
              let open Lwt.Syntax in
              let* () =
                enqueue_to_role ctx ~target_role:Pair_coding_types.Coder
                  ~message:notification
              in
              Lwt.return
                (Printf.sprintf "Note #%d recorded (%s, %s)." note_id
                   (Pair_coding_types.severity_to_string severity)
                   (match category with
                   | Some c -> Pair_coding_types.category_to_string c
                   | None -> "other"))
          | phase ->
              Lwt.return
                (Printf.sprintf
                   "Error: notes can only be written during coding or \
                    iteration phase. Current phase: %s."
                   (Pair_coding_types.phase_to_string phase))))
  | role ->
      Lwt.return
        (Printf.sprintf
           "Error: only the observer can write notes. Your role is %s."
           (Pair_coding_types.role_to_string role))

let action_approve ctx args =
  match ctx.role with
  | Pair_coding_types.Coordinator ->
      Lwt.return
        "Error: the coordinator cannot set approval. Only coder and observer \
         can approve."
  | role -> (
      let session = Pair_coding_state.load_session ~db:ctx.db ~id:ctx.pair_id in
      match session with
      | None -> Lwt.return "Error: pair session not found."
      | Some s -> (
          match s.phase with
          | Pair_coding_types.Review ->
              let open Yojson.Safe.Util in
              let approved =
                try args |> member "approved" |> to_bool
                with _ ->
                  failwith
                    "Error: 'approved' parameter (boolean) is required for \
                     approve. Set to true to approve, false to reject."
              in
              let comment =
                try args |> member "comment" |> to_string with _ -> ""
              in
              let both =
                Pair_coding_state.set_approval ~db:ctx.db ~id:ctx.pair_id ~role
                  ~approved ~comment
              in
              let status_word = if approved then "approved" else "rejected" in
              let notification =
                Printf.sprintf "[%s %s the current code]%s"
                  (Pair_coding_types.role_to_string role)
                  status_word
                  (if comment <> "" then ": " ^ comment else "")
              in
              let open Lwt.Syntax in
              (* Notify the other agent *)
              let other_role =
                match role with
                | Coder -> Pair_coding_types.Observer
                | _ -> Coder
              in
              let* () =
                enqueue_to_role ctx ~target_role:other_role
                  ~message:notification
              in
              (* If both approved, signal coordinator *)
              let* () =
                if both then begin
                  let* () =
                    enqueue_to_role ctx
                      ~target_role:Pair_coding_types.Coordinator
                      ~message:
                        "[BOTH_APPROVED] Both coder and observer have \
                         approved. Use pair_coding(action=\"signal\", \
                         transition=\"complete\") to finalize."
                  in
                  Lwt.return_unit
                end
                else Lwt.return_unit
              in
              Lwt.return
                (Printf.sprintf "%s recorded. %s"
                   (String.capitalize_ascii status_word)
                   (if both then "Both agents have approved!"
                    else "Waiting for other agent's approval."))
          | phase ->
              Lwt.return
                (Printf.sprintf
                   "Error: approval can only be given during review phase. \
                    Current phase: %s."
                   (Pair_coding_types.phase_to_string phase))))

let action_status ctx _args =
  let session = Pair_coding_state.load_session ~db:ctx.db ~id:ctx.pair_id in
  match session with
  | None -> Lwt.return "Error: pair session not found."
  | Some s ->
      let notes = Pair_coding_state.load_notes ~db:ctx.db ~session_id:s.id in
      let unresolved =
        List.filter (fun (n : Pair_coding_types.note) -> not n.resolved) notes
      in
      let blocking =
        List.filter
          (fun (n : Pair_coding_types.note) ->
            n.severity = Critical || n.severity = High)
          unresolved
      in
      let status =
        Printf.sprintf
          "Pair session: %s\n\
           Phase: %s\n\
           Review round: %d / %d\n\
           Task: %s\n\
           Notes: %d total, %d unresolved (%d blocking)\n\
           Coder approval: %s%s\n\
           Observer approval: %s%s\n\
           Your role: %s"
          s.id
          (Pair_coding_types.phase_to_string s.phase)
          s.review_round s.config.max_review_rounds s.config.task_description
          (List.length notes) (List.length unresolved) (List.length blocking)
          (if s.coder_approved then "approved" else "pending")
          (if s.coder_comment <> "" then " (" ^ s.coder_comment ^ ")" else "")
          (if s.observer_approved then "approved" else "pending")
          (if s.observer_comment <> "" then " (" ^ s.observer_comment ^ ")"
           else "")
          (Pair_coding_types.role_to_string ctx.role)
      in
      Lwt.return status

let action_signal ctx args =
  match ctx.role with
  | Pair_coding_types.Coordinator -> (
      let open Yojson.Safe.Util in
      let tr_str =
        try args |> member "transition" |> to_string
        with _ ->
          failwith
            "Error: 'transition' parameter is required for signal. One of: \
             start_review, start_iteration, complete, finalize, timeout, \
             abort."
      in
      match Pair_coding_types.transition_of_string tr_str with
      | None ->
          Lwt.return
            (Printf.sprintf
               "Error: invalid transition '%s'. Must be one of: start_review, \
                start_iteration, complete, finalize, timeout, abort."
               tr_str)
      | Some tr -> (
          let session =
            Pair_coding_state.load_session ~db:ctx.db ~id:ctx.pair_id
          in
          match session with
          | None -> Lwt.return "Error: pair session not found."
          | Some s -> (
              let cur_state =
                {
                  Pair_coding_types.phase = s.phase;
                  review_round = s.review_round;
                  max_review_rounds = s.config.max_review_rounds;
                  notes =
                    Pair_coding_state.load_notes ~db:ctx.db ~session_id:s.id;
                  coder_approval =
                    (if s.coder_approved then
                       Some
                         {
                           Pair_coding_types.approved = true;
                           comment = s.coder_comment;
                           timestamp_ms = 0;
                         }
                     else None);
                  observer_approval =
                    (if s.observer_approved then
                       Some
                         {
                           approved = true;
                           comment = s.observer_comment;
                           timestamp_ms = 0;
                         }
                     else None);
                  interrupts = 0;
                }
              in
              match Pair_coding_types.transition cur_state tr with
              | Error msg -> Lwt.return ("Error: " ^ msg)
              | Ok new_state ->
                  Pair_coding_state.update_phase ~db:ctx.db ~id:ctx.pair_id
                    new_state.phase;
                  (match tr with
                  | Start_review | Start_iteration ->
                      Pair_coding_state.update_review_round ~db:ctx.db
                        ~id:ctx.pair_id ~round:new_state.review_round;
                      Pair_coding_state.clear_approvals ~db:ctx.db
                        ~id:ctx.pair_id
                  | _ -> ());
                  if new_state.phase = Done then
                    Pair_coding_state.finish_session ~db:ctx.db ~id:ctx.pair_id;
                  let notification =
                    Printf.sprintf "[PHASE_CHANGE] Session phase changed to: %s"
                      (Pair_coding_types.phase_to_string new_state.phase)
                  in
                  let open Lwt.Syntax in
                  let* () =
                    enqueue_to_role ctx ~target_role:Pair_coding_types.Coder
                      ~message:notification
                  in
                  let* () =
                    enqueue_to_role ctx ~target_role:Pair_coding_types.Observer
                      ~message:notification
                  in
                  Lwt.return
                    (Printf.sprintf "Phase transitioned to %s."
                       (Pair_coding_types.phase_to_string new_state.phase)))))
  | role ->
      Lwt.return
        (Printf.sprintf
           "Error: only the coordinator can signal phase transitions. Your \
            role is %s."
           (Pair_coding_types.role_to_string role))

let action_request_swap ctx args =
  match ctx.role with
  | Pair_coding_types.Coordinator ->
      Lwt.return
        "Error: the coordinator cannot request a role swap. Only coder and \
         observer can."
  | role ->
      let open Yojson.Safe.Util in
      let reason =
        try args |> member "reason" |> to_string
        with _ ->
          failwith
            "Error: 'reason' parameter is required for request_swap. Explain \
             why roles should be swapped."
      in
      let notification =
        Printf.sprintf "[SWAP_REQUEST from %s]: %s"
          (Pair_coding_types.role_to_string role)
          reason
      in
      let open Lwt.Syntax in
      let* () =
        enqueue_to_role ctx ~target_role:Pair_coding_types.Coordinator
          ~message:notification
      in
      Lwt.return "Swap request sent to coordinator."

let action_resolve_note ctx args =
  match ctx.role with
  | Pair_coding_types.Coder -> (
      let session = Pair_coding_state.load_session ~db:ctx.db ~id:ctx.pair_id in
      match session with
      | None -> Lwt.return "Error: pair session not found."
      | Some s -> (
          match s.phase with
          | Pair_coding_types.Coding | Iteration ->
              let open Yojson.Safe.Util in
              let note_id =
                try args |> member "note_id" |> to_int
                with _ ->
                  failwith
                    "Error: 'note_id' (integer) is required for resolve_note. \
                     Use action=status to see note IDs."
              in
              (* Verify note exists and belongs to this session *)
              let notes =
                Pair_coding_state.load_notes ~db:ctx.db ~session_id:ctx.pair_id
              in
              let note_exists =
                List.exists
                  (fun (n : Pair_coding_types.note) -> n.id = note_id)
                  notes
              in
              if not note_exists then
                Lwt.return
                  (Printf.sprintf
                     "Error: note #%d not found in this session. Use \
                      action=status to see available note IDs."
                     note_id)
              else begin
                Pair_coding_state.resolve_note ~db:ctx.db ~note_id;
                let notification =
                  Printf.sprintf "[Note #%d resolved by coder]" note_id
                in
                let open Lwt.Syntax in
                let* () =
                  enqueue_to_role ctx ~target_role:Pair_coding_types.Observer
                    ~message:notification
                in
                Lwt.return
                  (Printf.sprintf "Note #%d marked as resolved." note_id)
              end
          | phase ->
              Lwt.return
                (Printf.sprintf
                   "Error: notes can only be resolved during coding or \
                    iteration phase. Current phase: %s."
                   (Pair_coding_types.phase_to_string phase))))
  | role ->
      Lwt.return
        (Printf.sprintf
           "Error: only the coder can resolve notes. Your role is %s. Use \
            send_msg to communicate with the coder."
           (Pair_coding_types.role_to_string role))

let invoke_pair_coding ctx args =
  let open Yojson.Safe.Util in
  let action =
    try args |> member "action" |> to_string
    with _ ->
      failwith
        "Error: 'action' parameter is required. Must be one of: send_msg, \
         write_note, approve, status, signal, request_swap, resolve_note."
  in
  Lwt.catch
    (fun () ->
      match action with
      | "send_msg" -> action_send_msg ctx args
      | "write_note" -> action_write_note ctx args
      | "approve" -> action_approve ctx args
      | "status" -> action_status ctx args
      | "signal" -> action_signal ctx args
      | "request_swap" -> action_request_swap ctx args
      | "resolve_note" -> action_resolve_note ctx args
      | _ ->
          Lwt.return
            (Printf.sprintf
               "Error: unknown action '%s'. Must be one of: send_msg, \
                write_note, approve, status, signal, request_swap, \
                resolve_note."
               action))
    (fun exn ->
      (* Catch failwith from parameter validation *)
      Lwt.return (Printexc.to_string exn))

let parameters_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ( "action",
              `Assoc
                [
                  ("type", `String "string");
                  ( "description",
                    `String
                      "Action to perform: send_msg, write_note, approve, \
                       status, signal, request_swap, resolve_note (required)" );
                  ( "enum",
                    `List
                      [
                        `String "send_msg";
                        `String "write_note";
                        `String "approve";
                        `String "status";
                        `String "signal";
                        `String "request_swap";
                        `String "resolve_note";
                      ] );
                ] );
            ( "to",
              `Assoc
                [
                  ("type", `String "string");
                  ( "description",
                    `String
                      "Target role for send_msg: coder, observer, or \
                       coordinator" );
                ] );
            ( "message",
              `Assoc
                [
                  ("type", `String "string");
                  ( "description",
                    `String "Message text (for send_msg and request_swap)" );
                ] );
            ( "description",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Note description (for write_note)");
                ] );
            ( "category",
              `Assoc
                [
                  ("type", `String "string");
                  ( "description",
                    `String
                      "Note category: bug, style, architecture, optimization, \
                       question, suggestion, security, other" );
                ] );
            ( "severity",
              `Assoc
                [
                  ("type", `String "string");
                  ( "description",
                    `String "Note severity: critical, high, medium, low" );
                ] );
            ( "file",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "File path for write_note");
                ] );
            ( "line",
              `Assoc
                [
                  ("type", `String "integer");
                  ("description", `String "Line number for write_note");
                ] );
            ( "approved",
              `Assoc
                [
                  ("type", `String "boolean");
                  ( "description",
                    `String "Approval decision (for approve action)" );
                ] );
            ( "comment",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Comment for approve action");
                ] );
            ( "transition",
              `Assoc
                [
                  ("type", `String "string");
                  ( "description",
                    `String
                      "Phase transition: start_review, start_iteration, \
                       complete, finalize, timeout, abort" );
                ] );
            ( "reason",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Reason for request_swap");
                ] );
            ( "note_id",
              `Assoc
                [
                  ("type", `String "integer");
                  ("description", `String "Note ID for resolve_note");
                ] );
          ] );
      ("required", `List [ `String "action" ]);
    ]

let make_tool ~(ctx : pair_context) : Tool.t =
  {
    name = "pair_coding";
    description =
      "Pair coding coordination tool. Communicate with pair coding partners, \
       manage observations, and control session workflow.";
    parameters_schema;
    invoke = (fun ?context:_ args -> invoke_pair_coding ctx args);
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }
