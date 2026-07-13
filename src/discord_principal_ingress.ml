(** Discord Gateway / interaction ingress principal derivation (P21.M1.E1.T007).

    Trust: authenticated Gateway WSS Ready session, or Interaction Ed25519
    signature over timestamp+body. Identity: immutable guild + user snowflakes.
    Bots, webhooks, DMs without guild, and missing ids fail closed. *)

type gateway_session = {
  session_id : string;
  application_id : string;
  ready : bool;
  last_seq : int option;
}

type human_identity = { guild_id : string; user_id : string }

type verified_context = {
  source : [ `Gateway | `Interaction ];
  application_id : string option;
  session_id : string option;
  seq : int option;
}

type outcome =
  | Human of {
      identity : human_identity;
      display_name : string option;
      context : verified_context;
    }
  | Bot_rejected of string
  | Invalid of string

let invalid msg = Invalid msg

let human_identity_key (h : human_identity) =
  Printf.sprintf "guild:%s:user:%s" h.guild_id h.user_id

let is_snowflake s =
  let t = String.trim s in
  let n = String.length t in
  n > 0
  &&
  let rec loop i =
    if i >= n then true
    else match t.[i] with '0' .. '9' -> loop (i + 1) | _ -> false
  in
  loop 0

let trim_nonempty s =
  let t = String.trim s in
  if t = "" then None else Some t

let json_string_field json name =
  let open Yojson.Safe.Util in
  try match member name json with `String s -> Some s | `Null | _ -> None
  with _ -> None

let json_bool_field json name =
  let open Yojson.Safe.Util in
  try match member name json with `Bool b -> Some b | _ -> None
  with _ -> None

let hex_decode s =
  let s = String.trim s in
  let n = String.length s in
  if n = 0 || n mod 2 <> 0 then
    Error "hex string must have even non-zero length"
  else
    let is_hex c =
      match c with '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false
    in
    let rec check i =
      if i >= n then true else if is_hex s.[i] then check (i + 1) else false
    in
    if not (check 0) then Error "hex string contains non-hex characters"
    else
      try
        let out = Bytes.create (n / 2) in
        for i = 0 to (n / 2) - 1 do
          let hi = s.[i * 2] and lo = s.[(i * 2) + 1] in
          let nibble c =
            match c with
            | '0' .. '9' -> Char.code c - Char.code '0'
            | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
            | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
            | _ -> raise Exit
          in
          Bytes.set out i (Char.chr ((nibble hi lsl 4) lor nibble lo))
        done;
        Ok (Bytes.unsafe_to_string out)
      with _ -> Error "hex decode failed"

let check_seq (s : gateway_session) seq =
  match (seq, s.last_seq) with
  | Some incoming, Some last when incoming < last ->
      Error
        (Printf.sprintf
           "dispatch sequence regression: seq=%d last_seq=%d (replay fail \
            closed)"
           incoming last)
  | Some incoming, _ when incoming < 0 ->
      Error "dispatch sequence must be non-negative"
  | _ -> Ok ()

let check_gateway_session ?expected_application_id ?seq (s : gateway_session) =
  if not s.ready then Error "gateway session not Ready (fail closed)"
  else
    match trim_nonempty s.session_id with
    | None -> Error "gateway session_id missing (fail closed)"
    | Some _ -> (
        match trim_nonempty s.application_id with
        | None -> Error "gateway application_id missing (fail closed)"
        | Some app_id -> (
            if not (is_snowflake app_id) then
              Error "gateway application_id is not a snowflake"
            else
              let app_ok =
                match expected_application_id with
                | Some exp when String.trim exp <> "" ->
                    let exp = String.trim exp in
                    if String.equal exp app_id then Ok ()
                    else
                      Error
                        (Printf.sprintf
                           "application_id mismatch: session=%s expected=%s"
                           app_id exp)
                | _ -> Ok ()
              in
              match app_ok with Error e -> Error e | Ok () -> check_seq s seq))

let reject_bot_or_webhook ~bot ~webhook_id =
  match webhook_id with
  | Some id when String.trim id <> "" ->
      Some "webhook identity cannot form a human principal"
  | _ -> if bot then Some "bot identity cannot form a human principal" else None

let make_human ~guild_id ~user_id ~display_name ~context =
  match (trim_nonempty guild_id, trim_nonempty user_id) with
  | None, _ -> invalid "missing guild_id snowflake (DM-ambiguous fail closed)"
  | _, None -> invalid "missing user_id snowflake (fail closed)"
  | Some guild_id, Some user_id ->
      if not (is_snowflake guild_id) then
        invalid "guild_id is not a Discord snowflake"
      else if not (is_snowflake user_id) then
        invalid "user_id is not a Discord snowflake"
      else
        Human
          {
            identity = { guild_id; user_id };
            display_name =
              (match display_name with
              | Some n -> trim_nonempty n
              | None -> None);
            context;
          }

let derive_from_fields ~session ?expected_application_id ?seq ~guild_id ~user_id
    ?(bot = false) ?webhook_id ?display_name () =
  match check_gateway_session ?expected_application_id ?seq session with
  | Error e -> invalid e
  | Ok () -> (
      match reject_bot_or_webhook ~bot ~webhook_id with
      | Some msg -> Bot_rejected msg
      | None ->
          let context =
            {
              source = `Gateway;
              application_id = Some session.application_id;
              session_id = Some session.session_id;
              seq;
            }
          in
          let guild = match guild_id with Some g -> g | None -> "" in
          let user = match user_id with Some u -> u | None -> "" in
          make_human ~guild_id:guild ~user_id:user
            ~display_name:
              (match display_name with Some d -> Some d | None -> None)
            ~context)

let extract_author payload =
  let open Yojson.Safe.Util in
  try
    let author = member "author" payload in
    match author with
    | `Null -> None
    | _ ->
        let id = json_string_field author "id" in
        let bot =
          match json_bool_field author "bot" with Some b -> b | None -> false
        in
        let username = json_string_field author "username" in
        let global_name = json_string_field author "global_name" in
        let display =
          match global_name with Some _ as g -> g | None -> username
        in
        Some (id, bot, display)
  with _ -> None

let derive_from_gateway ~session ?expected_application_id ?seq ?event_name
    ~payload_json () =
  let _ = event_name in
  match check_gateway_session ?expected_application_id ?seq session with
  | Error e -> invalid e
  | Ok () -> (
      let webhook_id = json_string_field payload_json "webhook_id" in
      match extract_author payload_json with
      | None -> invalid "payload missing author (fail closed)"
      | Some (user_id_opt, bot, display_name) -> (
          let guild_id = json_string_field payload_json "guild_id" in
          let display =
            match display_name with Some d -> Some d | None -> None
          in
          match webhook_id with
          | Some w ->
              derive_from_fields ~session ?expected_application_id ?seq
                ~guild_id ~user_id:user_id_opt ~bot ~webhook_id:w
                ?display_name:display ()
          | None ->
              derive_from_fields ~session ?expected_application_id ?seq
                ~guild_id ~user_id:user_id_opt ~bot ?display_name:display ()))

let verify_interaction_signature ~public_key_hex ~signature_hex ~timestamp ~body
    =
  let public_key_hex = String.trim public_key_hex in
  let signature_hex = String.trim signature_hex in
  let timestamp = String.trim timestamp in
  if public_key_hex = "" then Error "missing Discord application public key"
  else if signature_hex = "" then Error "missing X-Signature-Ed25519"
  else if timestamp = "" then Error "missing X-Signature-Timestamp"
  else
    match (hex_decode public_key_hex, hex_decode signature_hex) with
    | Error e, _ -> Error ("public key hex: " ^ e)
    | _, Error e -> Error ("signature hex: " ^ e)
    | Ok pk_bytes, Ok sig_bytes -> (
        if String.length pk_bytes <> 32 then
          Error
            (Printf.sprintf "Ed25519 public key must be 32 bytes, got %d"
               (String.length pk_bytes))
        else if String.length sig_bytes <> 64 then
          Error
            (Printf.sprintf "Ed25519 signature must be 64 bytes, got %d"
               (String.length sig_bytes))
        else
          match Mirage_crypto_ec.Ed25519.pub_of_octets pk_bytes with
          | Error _ -> Error "invalid Ed25519 public key octets"
          | Ok pub ->
              let msg = timestamp ^ body in
              if Mirage_crypto_ec.Ed25519.verify ~key:pub sig_bytes ~msg then
                Ok ()
              else Error "Ed25519 interaction signature verification failed")

let extract_interaction_user interaction_json =
  let open Yojson.Safe.Util in
  (* Prefer member.user in guild interactions; fall back to top-level user. *)
  let from_user_obj user =
    match user with
    | `Null -> None
    | _ ->
        let id = json_string_field user "id" in
        let bot =
          match json_bool_field user "bot" with Some b -> b | None -> false
        in
        let username = json_string_field user "username" in
        let global_name = json_string_field user "global_name" in
        let display =
          match global_name with Some _ as g -> g | None -> username
        in
        Some (id, bot, display)
  in
  try
    let member_obj = member "member" interaction_json in
    match member_obj with
    | `Null -> from_user_obj (member "user" interaction_json)
    | _ -> (
        match from_user_obj (member "user" member_obj) with
        | Some _ as u -> u
        | None -> from_user_obj (member "user" interaction_json))
  with _ -> (
    try from_user_obj (member "user" interaction_json) with _ -> None)

let derive_from_interaction ?public_key_hex ?signature_hex ?timestamp ?body
    ?(require_signature = true) ?expected_application_id ~interaction_json () =
  let sig_result =
    if not require_signature then Ok ()
    else
      match (public_key_hex, signature_hex, timestamp, body) with
      | Some pk, Some sig_, Some ts, Some b ->
          verify_interaction_signature ~public_key_hex:pk ~signature_hex:sig_
            ~timestamp:ts ~body:b
      | _ ->
          Error
            "interaction signature required but public_key, signature, \
             timestamp, or body missing (fail closed)"
  in
  match sig_result with
  | Error e -> invalid e
  | Ok () -> (
      let app_id =
        match json_string_field interaction_json "application_id" with
        | Some a -> Some (String.trim a)
        | None -> None
      in
      let app_check =
        match (expected_application_id, app_id) with
        | Some exp, Some got when String.trim exp <> "" ->
            if not (String.equal (String.trim exp) got) then
              Error
                (Printf.sprintf
                   "interaction application_id mismatch: got=%s expected=%s" got
                   (String.trim exp))
            else Ok ()
        | Some exp, None when String.trim exp <> "" ->
            Error "interaction missing application_id (fail closed)"
        | _ -> Ok ()
      in
      match app_check with
      | Error e -> invalid e
      | Ok () -> (
          match extract_interaction_user interaction_json with
          | None -> invalid "interaction missing user (fail closed)"
          | Some (user_id_opt, bot, display_name) -> (
              match reject_bot_or_webhook ~bot ~webhook_id:None with
              | Some msg -> Bot_rejected msg
              | None ->
                  let guild_id =
                    match json_string_field interaction_json "guild_id" with
                    | Some g -> g
                    | None -> ""
                  in
                  let user =
                    match user_id_opt with Some u -> u | None -> ""
                  in
                  let context =
                    {
                      source = `Interaction;
                      application_id = app_id;
                      session_id = None;
                      seq = None;
                    }
                  in
                  make_human ~guild_id ~user_id:user ~display_name ~context)))

let connector_actor_key_of_identity (h : human_identity) =
  Principal_identity.make_connector_actor_key ~connector:Discord
    ~tenant_or_workspace:h.guild_id ~immutable_user_id:h.user_id
