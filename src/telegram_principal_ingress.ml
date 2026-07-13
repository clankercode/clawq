(** Telegram Bot API ingress principal derivation (P21.M1.E1.T008).

    Long-poll trust boundary: bot-token-authenticated HTTPS [getUpdates].
    Webhook trust boundary: constant-time [secret_token] header check. Secrets
    authenticate the update only; identity is bot namespace + [from.id]. *)

type human_identity = { bot_namespace : string; user_id : string }
type chat_kind = Private | Group | Supergroup | Channel | Unknown of string
type chat_context = { chat_id : string; kind : chat_kind }

type outcome =
  | Human of {
      identity : human_identity;
      display_name : string option;
      username : string option;
      chat : chat_context option;
      update_id : int;
    }
  | Bot_rejected of string
  | Invalid of string
  | Stale_or_replay of { update_id : int; last_offset : int; message : string }

(* ---- offset store ---- *)

type offset_store = { last : (string, int) Hashtbl.t }

let create_offset_store ?(initial = []) () =
  let last = Hashtbl.create 8 in
  List.iter
    (fun (ns, uid) ->
      let ns = String.trim ns in
      if ns <> "" && uid >= 0 then Hashtbl.replace last ns uid)
    initial;
  { last }

let clear_offset_store (s : offset_store) = Hashtbl.clear s.last

let last_offset (s : offset_store) ~bot_namespace =
  Hashtbl.find_opt s.last (String.trim bot_namespace)

let advance_offset (s : offset_store) ~bot_namespace ~update_id =
  let ns = String.trim bot_namespace in
  if ns = "" || update_id < 0 then ()
  else
    match Hashtbl.find_opt s.last ns with
    | Some prev when update_id <= prev -> ()
    | _ -> Hashtbl.replace s.last ns update_id

let offset_store_to_list (s : offset_store) =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) s.last []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let mark_seen (s : offset_store) ~bot_namespace ~update_id =
  let ns = String.trim bot_namespace in
  match Hashtbl.find_opt s.last ns with
  | Some prev when update_id <= prev -> `Replay
  | _ ->
      if ns <> "" && update_id >= 0 then Hashtbl.replace s.last ns update_id;
      `New

(* ---- helpers ---- *)

let invalid msg = Invalid msg

let human_identity_key (h : human_identity) =
  Printf.sprintf "bot:%s:user:%s" h.bot_namespace h.user_id

let bot_namespace_of_token token =
  let t = String.trim token in
  match String.split_on_char ':' t with
  | bot_id :: _secret :: _ when bot_id <> "" ->
      (* Bot tokens are "<numeric_bot_id>:<secret>". Reject non-numeric prefixes. *)
      let is_digits =
        String.length bot_id > 0
        && String.for_all (function '0' .. '9' -> true | _ -> false) bot_id
      in
      if is_digits then Some bot_id else None
  | _ -> None

let verify_webhook_secret_token ~expected ~provided =
  let expected = String.trim expected in
  if expected = "" then false
  else
    match provided with
    | None -> false
    | Some p ->
        let p = String.trim p in
        if p = "" then false else Eqaf.equal expected p

let json_member name (json : Yojson.Safe.t) : Yojson.Safe.t option =
  match json with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member name json with `Null -> None | v -> Some v)
  | _ -> None

let json_string (v : Yojson.Safe.t) =
  match v with
  | `String s ->
      let t = String.trim s in
      if t = "" then None else Some t
  | _ -> None

let json_bool (v : Yojson.Safe.t) = match v with `Bool b -> Some b | _ -> None

let json_id (v : Yojson.Safe.t) =
  match v with
  | `Int i -> Some (string_of_int i)
  | `Intlit s ->
      let t = String.trim s in
      if t = "" then None else Some t
  | `Float f when Float.is_integer f -> Some (string_of_int (int_of_float f))
  | `String s ->
      let t = String.trim s in
      if t = "" then None
      else if
        String.for_all (function '0' .. '9' | '-' -> true | _ -> false) t
      then Some t
      else None
  | _ -> None

let member_id name json =
  match json_member name json with Some v -> json_id v | None -> None

let member_string name json =
  match json_member name json with Some v -> json_string v | None -> None

let member_bool name json =
  match json_member name json with Some v -> json_bool v | None -> None

let chat_kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "private" -> Private
  | "group" -> Group
  | "supergroup" -> Supergroup
  | "channel" -> Channel
  | other -> Unknown other

(* ---- sender / update extraction ---- *)

type sender = {
  user_id : string;
  is_bot : bool;
  first_name : string option;
  last_name : string option;
  username : string option;
}

let display_name_of_sender (s : sender) =
  match (s.first_name, s.last_name) with
  | Some f, Some l -> Some (String.trim (f ^ " " ^ l))
  | Some f, None -> Some f
  | None, Some l -> Some l
  | None, None -> s.username

let parse_user_object (u : Yojson.Safe.t) =
  match u with
  | `Assoc _ -> (
      match member_id "id" u with
      | None -> Error "user missing immutable id (from.id)"
      | Some user_id ->
          let is_bot =
            match member_bool "is_bot" u with Some b -> b | None -> false
          in
          Ok
            {
              user_id;
              is_bot;
              first_name = member_string "first_name" u;
              last_name = member_string "last_name" u;
              username = member_string "username" u;
            })
  | _ -> Error "missing user object"

let parse_chat_object (c : Yojson.Safe.t) =
  match c with
  | `Assoc _ -> (
      match member_id "id" c with
      | None -> None
      | Some chat_id ->
          let kind =
            match member_string "type" c with
            | Some t -> chat_kind_of_string t
            | None -> Unknown ""
          in
          Some { chat_id; kind })
  | _ -> None

(** Prefer message-like containers, then callback_query, then top-level [from].
*)
let extract_from_and_chat (update : Yojson.Safe.t) =
  let try_msg_field field =
    match json_member field update with
    | None | Some `Null -> None
    | Some msg ->
        let from =
          match json_member "from" msg with
          | None | Some `Null -> None
          | Some u -> Some (parse_user_object u)
        in
        let chat =
          match json_member "chat" msg with
          | None | Some `Null -> None
          | Some c -> parse_chat_object c
        in
        Some (from, chat)
  in
  let message_fields =
    [
      "message";
      "edited_message";
      "channel_post";
      "edited_channel_post";
      "my_chat_member";
      "chat_member";
      "chat_join_request";
    ]
  in
  let rec try_fields = function
    | [] -> None
    | f :: rest -> (
        match try_msg_field f with
        | Some (Some from_res, chat) -> Some (from_res, chat)
        | Some (None, _) | None -> try_fields rest)
  in
  match try_fields message_fields with
  | Some r -> r
  | None -> (
      match json_member "callback_query" update with
      | Some (`Assoc _ as cq) ->
          let from =
            match json_member "from" cq with
            | None | Some `Null -> Error "callback_query missing from"
            | Some u -> parse_user_object u
          in
          let chat =
            match json_member "message" cq with
            | Some msg -> (
                match json_member "chat" msg with
                | None | Some `Null -> None
                | Some c -> parse_chat_object c)
            | _ -> None
          in
          (from, chat)
      | _ -> (
          match json_member "inline_query" update with
          | Some (`Assoc _ as iq) ->
              let from =
                match json_member "from" iq with
                | None | Some `Null -> Error "inline_query missing from"
                | Some u -> parse_user_object u
              in
              (from, None)
          | _ -> (
              match json_member "poll_answer" update with
              | Some (`Assoc _ as pa) ->
                  let from =
                    match json_member "user" pa with
                    | None | Some `Null -> Error "poll_answer missing user"
                    | Some u -> parse_user_object u
                  in
                  (from, None)
              | _ -> (
                  match json_member "from" update with
                  | None | Some `Null ->
                      (Error "update missing immutable from.id sender", None)
                  | Some u -> (parse_user_object u, None)))))

let extract_update_id (update : Yojson.Safe.t) =
  match member_id "update_id" update with
  | Some s -> ( try Some (int_of_string s) with _ -> None)
  | None -> None

(* ---- core derive ---- *)

let derive ~bot_namespace ~update_json ~offset_store ~advance =
  let bot_namespace = String.trim bot_namespace in
  if bot_namespace = "" then invalid "bot_namespace must be non-empty"
  else
    match extract_update_id update_json with
    | None -> invalid "update missing update_id"
    | Some update_id when update_id < 0 ->
        invalid "update_id must be non-negative"
    | Some update_id -> (
        let check_offset () =
          match offset_store with
          | None -> Ok ()
          | Some store -> (
              match last_offset store ~bot_namespace with
              | Some last when update_id <= last ->
                  Error
                    (Stale_or_replay
                       {
                         update_id;
                         last_offset = last;
                         message =
                           Printf.sprintf
                             "stale or replayed update_id=%d (last_offset=%d)"
                             update_id last;
                       })
              | _ -> Ok ())
        in
        match check_offset () with
        | Error outcome -> outcome
        | Ok () -> (
            let maybe_advance outcome =
              (match (offset_store, advance, outcome) with
              | Some store, true, (Human _ | Bot_rejected _) ->
                  advance_offset store ~bot_namespace ~update_id
              | Some store, true, Invalid _ ->
                  (* Drain poison updates that still carry a valid update_id so
                     long-poll offsets keep advancing past unusable payloads. *)
                  advance_offset store ~bot_namespace ~update_id
              | _ -> ());
              outcome
            in
            match extract_from_and_chat update_json with
            | Error e, _ -> maybe_advance (invalid e)
            | Ok sender, chat ->
                if sender.is_bot then
                  maybe_advance
                    (Bot_rejected
                       "bot sender (is_bot=true) cannot form a human principal")
                else if sender.user_id = "" then
                  maybe_advance
                    (invalid "empty from.id (display fields are not identity)")
                else
                  maybe_advance
                    (Human
                       {
                         identity = { bot_namespace; user_id = sender.user_id };
                         display_name = display_name_of_sender sender;
                         username = sender.username;
                         chat;
                         update_id;
                       })))

let verify_and_derive_long_poll ?offset_store ?(advance = true) ~bot_namespace
    ~update_json () =
  let advance = match offset_store with None -> false | Some _ -> advance in
  derive ~bot_namespace ~update_json ~offset_store ~advance

let verify_and_derive_webhook ?offset_store ?(advance = true) ~bot_namespace
    ~expected_secret_token ~provided_secret_token ~update_json () =
  if
    not
      (verify_webhook_secret_token ~expected:expected_secret_token
         ~provided:provided_secret_token)
  then
    invalid
      "webhook secret_token missing or mismatch (authenticity fail closed)"
  else
    let advance = match offset_store with None -> false | Some _ -> advance in
    derive ~bot_namespace ~update_json ~offset_store ~advance
