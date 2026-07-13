(** Private cross-Connector linking and admin repair protocol (P21.M1.E1.T004).

    Pure types + validation only. Proof execution, adoption, unlink/split are
    later tasks. *)

module P = Principal_identity

let protocol_version = 1
let default_link_ttl_seconds = 900.0
let default_repair_ttl_seconds = 1800.0

(* -------------------------------------------------------------------------- *)
(* Link basis                                                                 *)
(* -------------------------------------------------------------------------- *)

type link_basis =
  | Two_sided_private_proof
  | Admin_repair
  | Auto_display_name
  | Auto_email
  | Auto_external_account

let string_of_link_basis = function
  | Two_sided_private_proof -> "two_sided_private_proof"
  | Admin_repair -> "admin_repair"
  | Auto_display_name -> "auto_display_name"
  | Auto_email -> "auto_email"
  | Auto_external_account -> "auto_external_account"

let link_basis_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "two_sided_private_proof" -> Ok Two_sided_private_proof
  | "admin_repair" -> Ok Admin_repair
  | "auto_display_name" -> Ok Auto_display_name
  | "auto_email" -> Ok Auto_email
  | "auto_external_account" -> Ok Auto_external_account
  | other -> Error (Printf.sprintf "unknown link_basis: %s" other)

let link_basis_is_allowed = function
  | Two_sided_private_proof | Admin_repair -> true
  | Auto_display_name | Auto_email | Auto_external_account -> false

let assert_link_basis_allowed basis =
  if link_basis_is_allowed basis then Ok ()
  else
    Error
      (Printf.sprintf
         "link_basis %S is forbidden: display names, emails, and matching \
          external accounts never auto-merge Principals; use \
          two_sided_private_proof or admin_repair"
         (string_of_link_basis basis))

(* -------------------------------------------------------------------------- *)
(* Verified endpoint                                                          *)
(* -------------------------------------------------------------------------- *)

type verified_endpoint = {
  actor_key : P.connector_actor_key;
  principal_id : P.principal_id option;
  principal_revision : int option;
  actor_revision : int;
  verified_at : string;
}

let make_verified_endpoint ~actor_key ?principal_id ?principal_revision
    ?(actor_revision = 1) ~verified_at () =
  let verified_at = String.trim verified_at in
  if verified_at = "" then
    Error
      "verified_at must be non-empty: only adapter-verified Connector actors \
       may be link endpoints"
  else if actor_revision <= 0 then
    Error "actor_revision must be a positive integer"
  else
    match principal_id with
    | None ->
        if principal_revision <> None then
          Error "principal_revision requires principal_id"
        else
          Ok
            {
              actor_key;
              principal_id = None;
              principal_revision = None;
              actor_revision;
              verified_at;
            }
    | Some pid ->
        let rev = match principal_revision with Some r -> r | None -> 1 in
        if rev <= 0 then Error "principal_revision must be a positive integer"
        else
          Ok
            {
              actor_key;
              principal_id = Some pid;
              principal_revision = Some rev;
              actor_revision;
              verified_at;
            }

let endpoints_distinct a b =
  not (P.connector_actor_key_equal a.actor_key b.actor_key)

let require_two_verified_endpoints a b =
  if String.trim a.verified_at = "" || String.trim b.verified_at = "" then
    Error "both endpoints must carry non-empty verified_at from trusted ingress"
  else if not (endpoints_distinct a b) then
    Error
      "link requires two distinct Connector actor endpoints; the same actor \
       cannot link to itself"
  else Ok ()

(* -------------------------------------------------------------------------- *)
(* Private proof delivery                                                     *)
(* -------------------------------------------------------------------------- *)

type private_delivery_channel =
  | Connector_dm of { connector : P.connector; handle_id : string }
  | Web_private of { handle_id : string }
  | Cli_private of { handle_id : string }
  | Unsupported of { reason : string }

type private_proof_delivery = {
  channel : private_delivery_channel;
  delivery_id : string;
  endpoint_side : [ `A | `B ];
  created_at : string;
}

let validate_channel = function
  | Unsupported { reason } ->
      if String.trim reason = "" then
        Error "Unsupported delivery reason must be non-empty"
      else Ok ()
  | Connector_dm { handle_id; _ }
  | Web_private { handle_id }
  | Cli_private { handle_id } ->
      if String.trim handle_id = "" then
        Error
          "private delivery handle_id must be a non-empty opaque alias (never \
           a proof secret)"
      else Ok ()

let make_private_proof_delivery ~channel ~delivery_id ~endpoint_side
    ?(created_at = "") () =
  let delivery_id = String.trim delivery_id in
  if delivery_id = "" then Error "delivery_id must be non-empty"
  else
    match validate_channel channel with
    | Error e -> Error e
    | Ok () ->
        let created_at =
          if String.trim created_at = "" then Time_util.iso8601_utc ()
          else String.trim created_at
        in
        Ok { channel; delivery_id; endpoint_side; created_at }

let delivery_is_export_safe (d : private_proof_delivery) =
  match validate_channel d.channel with Ok () -> true | Error _ -> false

(* -------------------------------------------------------------------------- *)
(* Link transaction status                                                    *)
(* -------------------------------------------------------------------------- *)

type link_tx_status =
  | Open
  | Awaiting_counterpart
  | Completed
  | Expired
  | Cancelled
  | Superseded

let string_of_link_tx_status = function
  | Open -> "open"
  | Awaiting_counterpart -> "awaiting_counterpart"
  | Completed -> "completed"
  | Expired -> "expired"
  | Cancelled -> "cancelled"
  | Superseded -> "superseded"

let link_tx_status_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "open" -> Ok Open
  | "awaiting_counterpart" -> Ok Awaiting_counterpart
  | "completed" -> Ok Completed
  | "expired" -> Ok Expired
  | "cancelled" -> Ok Cancelled
  | "superseded" -> Ok Superseded
  | other -> Error (Printf.sprintf "unknown link_tx_status: %s" other)

let link_tx_status_is_terminal = function
  | Completed | Expired | Cancelled | Superseded -> true
  | Open | Awaiting_counterpart -> false

let link_tx_status_accepts_proof = function
  | Open | Awaiting_counterpart -> true
  | Completed | Expired | Cancelled | Superseded -> false

(* -------------------------------------------------------------------------- *)
(* Link transaction                                                           *)
(* -------------------------------------------------------------------------- *)

type link_transaction = {
  version : int;
  id : string;
  basis : link_basis;
  endpoint_a : verified_endpoint;
  endpoint_b : verified_endpoint;
  initiator : [ `A | `B ];
  status : link_tx_status;
  replay_protection_id : string;
  proof_challenge_id : string;
  a_proved : bool;
  b_proved : bool;
  delivery_a : private_proof_delivery option;
  delivery_b : private_proof_delivery option;
  created_at : string;
  expires_at : string;
  completed_at : string option;
  cancelled_at : string option;
  cancel_reason : string option;
}

let iso_now ?(now = Unix.gettimeofday ()) () = Time_util.iso8601_utc ~t:now ()

let require_nonempty name s =
  let t = String.trim s in
  if t = "" then Error (Printf.sprintf "%s must be non-empty" name) else Ok t

let status_matches_proof_flags status a_proved b_proved =
  match (status, a_proved, b_proved) with
  | Open, false, false -> Ok ()
  | Awaiting_counterpart, true, false | Awaiting_counterpart, false, true ->
      Ok ()
  | Completed, true, true -> Ok ()
  | (Expired | Cancelled | Superseded), _, _ -> Ok ()
  | Open, _, _ -> Error "status open requires neither endpoint proved"
  | Awaiting_counterpart, _, _ ->
      Error "status awaiting_counterpart requires exactly one endpoint proved"
  | Completed, _, _ -> Error "status completed requires both endpoints proved"

let validate_link_transaction (tx : link_transaction) =
  if tx.version <> protocol_version then
    Error
      (Printf.sprintf "unsupported link transaction version %d (expected %d)"
         tx.version protocol_version)
  else
    match require_nonempty "id" tx.id with
    | Error e -> Error e
    | Ok _ -> (
        match
          require_nonempty "replay_protection_id" tx.replay_protection_id
        with
        | Error e -> Error e
        | Ok _ -> (
            match
              require_nonempty "proof_challenge_id" tx.proof_challenge_id
            with
            | Error e -> Error e
            | Ok _ -> (
                match assert_link_basis_allowed tx.basis with
                | Error e -> Error e
                | Ok () -> (
                    if tx.basis <> Two_sided_private_proof then
                      Error
                        "ordinary link_transaction basis must be \
                         two_sided_private_proof (admin_repair uses \
                         admin_repair_plan)"
                    else
                      match
                        require_two_verified_endpoints tx.endpoint_a
                          tx.endpoint_b
                      with
                      | Error e -> Error e
                      | Ok () -> (
                          match
                            status_matches_proof_flags tx.status tx.a_proved
                              tx.b_proved
                          with
                          | Error e -> Error e
                          | Ok () ->
                              if String.trim tx.created_at = "" then
                                Error "created_at must be non-empty"
                              else if String.trim tx.expires_at = "" then
                                Error "expires_at must be non-empty"
                              else if
                                String.compare tx.expires_at tx.created_at <= 0
                              then Error "expires_at must be after created_at"
                              else Ok ())))))

let make_link_transaction ~id ~endpoint_a ~endpoint_b ?(initiator = `A)
    ~replay_protection_id ~proof_challenge_id
    ?(ttl_seconds = default_link_ttl_seconds) ?now ?created_at ?expires_at () =
  if ttl_seconds <= 0. && created_at = None then
    Error "ttl_seconds must be positive"
  else
    let now = match now with Some t -> t | None -> Unix.gettimeofday () in
    let created_at =
      match created_at with Some s -> String.trim s | None -> iso_now ~now ()
    in
    let expires_at =
      match expires_at with
      | Some s -> String.trim s
      | None -> Time_util.iso8601_utc ~t:(now +. ttl_seconds) ()
    in
    let tx =
      {
        version = protocol_version;
        id = String.trim id;
        basis = Two_sided_private_proof;
        endpoint_a;
        endpoint_b;
        initiator;
        status = Open;
        replay_protection_id = String.trim replay_protection_id;
        proof_challenge_id = String.trim proof_challenge_id;
        a_proved = false;
        b_proved = false;
        delivery_a = None;
        delivery_b = None;
        created_at;
        expires_at;
        completed_at = None;
        cancelled_at = None;
        cancel_reason = None;
      }
    in
    match validate_link_transaction tx with
    | Error e -> Error e
    | Ok () -> Ok tx

let link_transaction_is_expired ?(now = Unix.gettimeofday ())
    (tx : link_transaction) =
  let now_s = iso_now ~now () in
  String.compare now_s tx.expires_at > 0

let assert_not_cancelled (tx : link_transaction) =
  match tx.status with
  | Cancelled ->
      Error
        (Printf.sprintf "link transaction %s is cancelled%s" tx.id
           (match tx.cancel_reason with Some r -> ": " ^ r | None -> ""))
  | _ -> Ok ()

let assert_link_open_for_proof ?(now = Unix.gettimeofday ())
    (tx : link_transaction) =
  match assert_not_cancelled tx with
  | Error e -> Error e
  | Ok () ->
      if not (link_tx_status_accepts_proof tx.status) then
        Error
          (Printf.sprintf "link transaction %s status %s does not accept proof"
             tx.id
             (string_of_link_tx_status tx.status))
      else if link_transaction_is_expired ~now tx then
        Error
          (Printf.sprintf "link transaction %s expired at %s" tx.id
             tx.expires_at)
      else Ok ()

type replay_check = Fresh | Idempotent_completed | Rejected of string

let check_replay (tx : link_transaction) ~presented_replay_id =
  let presented = String.trim presented_replay_id in
  if presented = "" then Rejected "presented_replay_id must be non-empty"
  else if not (String.equal presented tx.replay_protection_id) then
    Rejected "replay_protection_id mismatch"
  else
    match tx.status with
    | Completed -> Idempotent_completed
    | Open | Awaiting_counterpart -> Fresh
    | Expired ->
        Rejected
          (Printf.sprintf "cannot replay expired link transaction %s" tx.id)
    | Cancelled ->
        Rejected
          (Printf.sprintf "cannot replay cancelled link transaction %s" tx.id)
    | Superseded ->
        Rejected
          (Printf.sprintf "cannot replay superseded link transaction %s" tx.id)

let mark_endpoint_proved_pure (tx : link_transaction) ~side
    ?(now = Unix.gettimeofday ()) () =
  match assert_link_open_for_proof ~now tx with
  | Error e -> Error e
  | Ok () ->
      let already = match side with `A -> tx.a_proved | `B -> tx.b_proved in
      if already then
        Error
          (Printf.sprintf "endpoint %s already proved on transaction %s"
             (match side with `A -> "A" | `B -> "B")
             tx.id)
      else
        let a_proved = match side with `A -> true | `B -> tx.a_proved in
        let b_proved = match side with `B -> true | `A -> tx.b_proved in
        let status, completed_at =
          match (a_proved, b_proved) with
          | true, true -> (Completed, Some (iso_now ~now ()))
          | true, false | false, true -> (Awaiting_counterpart, None)
          | false, false -> (Open, None)
        in
        Ok { tx with a_proved; b_proved; status; completed_at }

let cancel_link_transaction_pure (tx : link_transaction) ?reason
    ?(now = Unix.gettimeofday ()) () =
  if link_tx_status_is_terminal tx.status then
    Error
      (Printf.sprintf "cannot cancel terminal link transaction %s (status %s)"
         tx.id
         (string_of_link_tx_status tx.status))
  else
    Ok
      {
        tx with
        status = Cancelled;
        cancelled_at = Some (iso_now ~now ());
        cancel_reason = reason;
      }

let expire_link_transaction_pure (tx : link_transaction)
    ?(now = Unix.gettimeofday ()) () =
  if link_tx_status_is_terminal tx.status then
    Error
      (Printf.sprintf "cannot expire terminal link transaction %s (status %s)"
         tx.id
         (string_of_link_tx_status tx.status))
  else if not (link_transaction_is_expired ~now tx) then
    Error
      (Printf.sprintf "link transaction %s has not expired (expires_at %s)"
         tx.id tx.expires_at)
  else Ok { tx with status = Expired }

(* -------------------------------------------------------------------------- *)
(* Admin repair                                                               *)
(* -------------------------------------------------------------------------- *)

type repair_status =
  | Planned
  | Confirmed
  | Applied
  | Rejected
  | Expired
  | Cancelled
  | Stale_revision

let string_of_repair_status = function
  | Planned -> "planned"
  | Confirmed -> "confirmed"
  | Applied -> "applied"
  | Rejected -> "rejected"
  | Expired -> "expired"
  | Cancelled -> "cancelled"
  | Stale_revision -> "stale_revision"

let repair_status_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "planned" -> Ok Planned
  | "confirmed" -> Ok Confirmed
  | "applied" -> Ok Applied
  | "rejected" -> Ok Rejected
  | "expired" -> Ok Expired
  | "cancelled" -> Ok Cancelled
  | "stale_revision" -> Ok Stale_revision
  | other -> Error (Printf.sprintf "unknown repair_status: %s" other)

let repair_status_is_terminal = function
  | Applied | Rejected | Expired | Cancelled | Stale_revision -> true
  | Planned | Confirmed -> false

type survivor_selection = By_creation_order | Explicit of P.principal_id

type repair_conflict =
  | External_account_collision of { summary : string }
  | Preference_conflict of { key : string; summary : string }
  | Pending_authorization_invalidated of { count : int }
  | Other of { code : string; summary : string }

type repair_preview = {
  survivor_principal_id : P.principal_id option;
  merged_principal_id : P.principal_id option;
  conflicts : repair_conflict list;
  notes : string list;
}

type admin_repair_plan = {
  version : int;
  id : string;
  basis : link_basis;
  endpoint_a : verified_endpoint;
  endpoint_b : verified_endpoint;
  admin_principal_id : P.principal_id;
  survivor : survivor_selection;
  base_principal_a_revision : int option;
  base_principal_b_revision : int option;
  preview : repair_preview;
  digest : string;
  status : repair_status;
  created_at : string;
  expires_at : string;
  confirmed_at : string option;
  applied_at : string option;
  reject_reason : string option;
}

let conflict_to_json = function
  | External_account_collision { summary } ->
      `Assoc
        [
          ("kind", `String "external_account_collision");
          ("summary", `String summary);
        ]
  | Preference_conflict { key; summary } ->
      `Assoc
        [
          ("kind", `String "preference_conflict");
          ("key", `String key);
          ("summary", `String summary);
        ]
  | Pending_authorization_invalidated { count } ->
      `Assoc
        [
          ("kind", `String "pending_authorization_invalidated");
          ("count", `Int count);
        ]
  | Other { code; summary } ->
      `Assoc
        [
          ("kind", `String "other");
          ("code", `String code);
          ("summary", `String summary);
        ]

let preview_to_json (p : repair_preview) =
  let opt_pid = function
    | None -> `Null
    | Some id -> `String (P.principal_id_to_string id)
  in
  `Assoc
    [
      ("survivor_principal_id", opt_pid p.survivor_principal_id);
      ("merged_principal_id", opt_pid p.merged_principal_id);
      ("conflicts", `List (List.map conflict_to_json p.conflicts));
      ("notes", `List (List.map (fun s -> `String s) p.notes));
    ]

let survivor_to_json = function
  | By_creation_order -> `Assoc [ ("kind", `String "by_creation_order") ]
  | Explicit id ->
      `Assoc
        [
          ("kind", `String "explicit");
          ("principal_id", `String (P.principal_id_to_string id));
        ]

let verified_endpoint_to_json (e : verified_endpoint) =
  let fields =
    [
      ("actor_key", P.connector_actor_key_to_json e.actor_key);
      ("actor_revision", `Int e.actor_revision);
      ("verified_at", `String e.verified_at);
    ]
  in
  let fields =
    match e.principal_id with
    | None -> ("principal_id", `Null) :: fields
    | Some id ->
        ("principal_id", `String (P.principal_id_to_string id)) :: fields
  in
  let fields =
    match e.principal_revision with
    | None -> ("principal_revision", `Null) :: fields
    | Some r -> ("principal_revision", `Int r) :: fields
  in
  `Assoc fields

let side_to_string = function `A -> "a" | `B -> "b"

let channel_to_json = function
  | Connector_dm { connector; handle_id } ->
      `Assoc
        [
          ("kind", `String "connector_dm");
          ("connector", `String (P.string_of_connector connector));
          ("handle_id", `String handle_id);
        ]
  | Web_private { handle_id } ->
      `Assoc
        [ ("kind", `String "web_private"); ("handle_id", `String handle_id) ]
  | Cli_private { handle_id } ->
      `Assoc
        [ ("kind", `String "cli_private"); ("handle_id", `String handle_id) ]
  | Unsupported { reason } ->
      `Assoc [ ("kind", `String "unsupported"); ("reason", `String reason) ]

let private_proof_delivery_to_json (d : private_proof_delivery) =
  `Assoc
    [
      ("channel", channel_to_json d.channel);
      ("delivery_id", `String d.delivery_id);
      ("endpoint_side", `String (side_to_string d.endpoint_side));
      ("created_at", `String d.created_at);
    ]

let sort_assoc_keys = function
  | `Assoc fields ->
      `Assoc (List.sort (fun (a, _) (b, _) -> String.compare a b) fields)
  | other -> other

let digest_hex payload =
  let open Digestif.SHA256 in
  let d = digest_string payload in
  to_hex d

let repair_canonical_body (plan : admin_repair_plan) =
  (* Digest binds identity + endpoints + revision + preview only. Status
     transitions (confirm/apply) and timestamps of those transitions are
     excluded so the confirm digest remains stable. *)
  sort_assoc_keys
    (`Assoc
       [
         ("version", `Int plan.version);
         ("id", `String plan.id);
         ("basis", `String (string_of_link_basis plan.basis));
         ("endpoint_a", verified_endpoint_to_json plan.endpoint_a);
         ("endpoint_b", verified_endpoint_to_json plan.endpoint_b);
         ( "admin_principal_id",
           `String (P.principal_id_to_string plan.admin_principal_id) );
         ("survivor", survivor_to_json plan.survivor);
         ( "base_principal_a_revision",
           match plan.base_principal_a_revision with
           | None -> `Null
           | Some r -> `Int r );
         ( "base_principal_b_revision",
           match plan.base_principal_b_revision with
           | None -> `Null
           | Some r -> `Int r );
         ("preview", preview_to_json plan.preview);
         ("created_at", `String plan.created_at);
         ("expires_at", `String plan.expires_at);
       ])

let compute_repair_digest (plan : admin_repair_plan) =
  let body = repair_canonical_body plan in
  digest_hex (Yojson.Safe.to_string body)

let validate_survivor endpoint_a endpoint_b survivor =
  match survivor with
  | By_creation_order -> Ok ()
  | Explicit sid -> (
      let ids =
        List.filter_map
          (fun (e : verified_endpoint) -> e.principal_id)
          [ endpoint_a; endpoint_b ]
      in
      match ids with
      | [] ->
          Error
            "explicit survivor requires at least one endpoint with a \
             principal_id"
      | _ ->
          if List.exists (fun id -> P.principal_id_equal id sid) ids then Ok ()
          else
            Error "explicit survivor must be one of the endpoint Principal ids")

let validate_admin_repair_plan (plan : admin_repair_plan) =
  if plan.version <> protocol_version then
    Error
      (Printf.sprintf "unsupported repair plan version %d (expected %d)"
         plan.version protocol_version)
  else
    match require_nonempty "id" plan.id with
    | Error e -> Error e
    | Ok _ -> (
        match assert_link_basis_allowed plan.basis with
        | Error e -> Error e
        | Ok () -> (
            if plan.basis <> Admin_repair then
              Error "admin_repair_plan basis must be admin_repair"
            else
              match
                require_two_verified_endpoints plan.endpoint_a plan.endpoint_b
              with
              | Error e -> Error e
              | Ok () -> (
                  match
                    validate_survivor plan.endpoint_a plan.endpoint_b
                      plan.survivor
                  with
                  | Error e -> Error e
                  | Ok () -> (
                      let rev_ok side (e : verified_endpoint) bound =
                        match (e.principal_id, bound) with
                        | None, None -> Ok ()
                        | Some _, Some r when r > 0 -> Ok ()
                        | Some _, Some _ ->
                            Error
                              (side
                             ^ " base_principal revision must be positive")
                        | Some _, None ->
                            Error
                              (side
                             ^ " endpoint has principal_id but missing \
                                base_principal revision binding")
                        | None, Some _ ->
                            Error
                              (side
                             ^ " base_principal revision without principal_id")
                      in
                      match
                        rev_ok "endpoint_a" plan.endpoint_a
                          plan.base_principal_a_revision
                      with
                      | Error e -> Error e
                      | Ok () -> (
                          match
                            rev_ok "endpoint_b" plan.endpoint_b
                              plan.base_principal_b_revision
                          with
                          | Error e -> Error e
                          | Ok () ->
                              if String.trim plan.created_at = "" then
                                Error "created_at must be non-empty"
                              else if String.trim plan.expires_at = "" then
                                Error "expires_at must be non-empty"
                              else if
                                String.compare plan.expires_at plan.created_at
                                <= 0
                              then Error "expires_at must be after created_at"
                              else if String.trim plan.digest = "" then
                                Error "digest must be non-empty"
                              else
                                let expected = compute_repair_digest plan in
                                if not (String.equal expected plan.digest) then
                                  Error "repair plan digest mismatch"
                                else Ok ())))))

let make_admin_repair_plan ~id ~endpoint_a ~endpoint_b ~admin_principal_id
    ~survivor ~preview ?(ttl_seconds = default_repair_ttl_seconds) ?now
    ?created_at ?expires_at () =
  if ttl_seconds <= 0. && created_at = None then
    Error "ttl_seconds must be positive"
  else
    let now = match now with Some t -> t | None -> Unix.gettimeofday () in
    let created_at =
      match created_at with Some s -> String.trim s | None -> iso_now ~now ()
    in
    let expires_at =
      match expires_at with
      | Some s -> String.trim s
      | None -> Time_util.iso8601_utc ~t:(now +. ttl_seconds) ()
    in
    let base_a = endpoint_a.principal_revision in
    let base_b = endpoint_b.principal_revision in
    let draft =
      {
        version = protocol_version;
        id = String.trim id;
        basis = Admin_repair;
        endpoint_a;
        endpoint_b;
        admin_principal_id;
        survivor;
        base_principal_a_revision = base_a;
        base_principal_b_revision = base_b;
        preview;
        digest = "";
        status = Planned;
        created_at;
        expires_at;
        confirmed_at = None;
        applied_at = None;
        reject_reason = None;
      }
    in
    let digest = compute_repair_digest draft in
    let plan = { draft with digest } in
    match validate_admin_repair_plan plan with
    | Error e -> Error e
    | Ok () -> Ok plan

let admin_repair_is_expired ?(now = Unix.gettimeofday ())
    (plan : admin_repair_plan) =
  let now_s = iso_now ~now () in
  String.compare now_s plan.expires_at > 0

let digests_equal a b =
  let a = String.trim a and b = String.trim b in
  let len_a = String.length a and len_b = String.length b in
  if len_a <> len_b then false
  else
    let acc = ref 0 in
    for i = 0 to len_a - 1 do
      acc := !acc lor (Char.code a.[i] lxor Char.code b.[i])
    done;
    !acc = 0

let confirm_repair_plan_pure (plan : admin_repair_plan) ~presented_digest
    ~confirming_principal ?(now = Unix.gettimeofday ()) () =
  match plan.status with
  | Planned ->
      if admin_repair_is_expired ~now plan then
        Error
          (Printf.sprintf "repair plan %s expired at %s" plan.id plan.expires_at)
      else if
        not (P.principal_id_equal confirming_principal plan.admin_principal_id)
      then Error "confirming principal does not match repair plan admin"
      else if not (digests_equal presented_digest plan.digest) then
        Error "repair plan digest mismatch on confirm"
      else
        Ok
          {
            plan with
            status = Confirmed;
            confirmed_at = Some (iso_now ~now ());
          }
  | Confirmed -> Ok plan
  | other ->
      Error
        (Printf.sprintf "cannot confirm repair plan in status %s"
           (string_of_repair_status other))

let mark_repair_applied_pure (plan : admin_repair_plan)
    ?(now = Unix.gettimeofday ()) () =
  match plan.status with
  | Applied -> Ok plan
  | Confirmed ->
      if admin_repair_is_expired ~now plan then
        Error
          (Printf.sprintf "repair plan %s expired at %s" plan.id plan.expires_at)
      else
        Ok { plan with status = Applied; applied_at = Some (iso_now ~now ()) }
  | other ->
      Error
        (Printf.sprintf "cannot apply repair plan in status %s"
           (string_of_repair_status other))

let reject_repair_plan_pure (plan : admin_repair_plan) ~reason () =
  if repair_status_is_terminal plan.status && plan.status <> Rejected then
    Error
      (Printf.sprintf "cannot reject terminal repair plan %s (status %s)"
         plan.id
         (string_of_repair_status plan.status))
  else
    Ok
      { plan with status = Rejected; reject_reason = Some (String.trim reason) }

let cancel_repair_plan_pure (plan : admin_repair_plan)
    ?(now = Unix.gettimeofday ()) () =
  if repair_status_is_terminal plan.status then
    Error
      (Printf.sprintf "cannot cancel terminal repair plan %s (status %s)"
         plan.id
         (string_of_repair_status plan.status))
  else
    Ok
      {
        plan with
        status = Cancelled;
        reject_reason =
          Some (Printf.sprintf "cancelled_at=%s" (iso_now ~now ()));
      }

(* -------------------------------------------------------------------------- *)
(* Auto-link rejection                                                        *)
(* -------------------------------------------------------------------------- *)

type auto_link_proposal = {
  basis : link_basis;
  display_name : string option;
  email : string option;
  external_account_hint : string option;
  left_actor : P.connector_actor_key option;
  right_actor : P.connector_actor_key option;
}

let reject_auto_link (p : auto_link_proposal) =
  (* Returns [Error] when the proposal is an auto-link or incomplete evidence
     that must not establish identity. Returns [Ok ()] only when basis is
     allowed and two distinct actor keys are present (caller still builds a
     proper link_transaction or admin_repair_plan). *)
  match assert_link_basis_allowed p.basis with
  | Error e -> Error e
  | Ok () -> (
      match (p.left_actor, p.right_actor) with
      | Some a, Some b when not (P.connector_actor_key_equal a b) -> Ok ()
      | Some a, Some b when P.connector_actor_key_equal a b ->
          Error
            "link requires two distinct Connector actor endpoints; the same \
             actor cannot link to itself"
      | _ ->
          let has_hint =
            Option.is_some p.display_name
            || Option.is_some p.email
            || Option.is_some p.external_account_hint
          in
          if has_hint then
            Error
              "display names, emails, and external-account hints never \
               auto-merge Principals; require two verified Connector actor \
               endpoints and two_sided_private_proof or admin_repair"
          else
            Error
              "cross-Connector link requires two verified Connector actor \
               endpoints")

(* -------------------------------------------------------------------------- *)
(* Redacted audit                                                             *)
(* -------------------------------------------------------------------------- *)

type audit_kind =
  | Link_tx_created
  | Link_proof_delivered
  | Link_endpoint_proved
  | Link_tx_completed
  | Link_tx_expired
  | Link_tx_cancelled
  | Link_tx_replayed
  | Link_tx_superseded
  | Repair_planned
  | Repair_confirmed
  | Repair_applied
  | Repair_rejected
  | Repair_cancelled
  | Repair_expired
  | Repair_stale_revision
  | Auto_link_rejected

let string_of_audit_kind = function
  | Link_tx_created -> "link_tx_created"
  | Link_proof_delivered -> "link_proof_delivered"
  | Link_endpoint_proved -> "link_endpoint_proved"
  | Link_tx_completed -> "link_tx_completed"
  | Link_tx_expired -> "link_tx_expired"
  | Link_tx_cancelled -> "link_tx_cancelled"
  | Link_tx_replayed -> "link_tx_replayed"
  | Link_tx_superseded -> "link_tx_superseded"
  | Repair_planned -> "repair_planned"
  | Repair_confirmed -> "repair_confirmed"
  | Repair_applied -> "repair_applied"
  | Repair_rejected -> "repair_rejected"
  | Repair_cancelled -> "repair_cancelled"
  | Repair_expired -> "repair_expired"
  | Repair_stale_revision -> "repair_stale_revision"
  | Auto_link_rejected -> "auto_link_rejected"

let audit_kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "link_tx_created" -> Ok Link_tx_created
  | "link_proof_delivered" -> Ok Link_proof_delivered
  | "link_endpoint_proved" -> Ok Link_endpoint_proved
  | "link_tx_completed" -> Ok Link_tx_completed
  | "link_tx_expired" -> Ok Link_tx_expired
  | "link_tx_cancelled" -> Ok Link_tx_cancelled
  | "link_tx_replayed" -> Ok Link_tx_replayed
  | "link_tx_superseded" -> Ok Link_tx_superseded
  | "repair_planned" -> Ok Repair_planned
  | "repair_confirmed" -> Ok Repair_confirmed
  | "repair_applied" -> Ok Repair_applied
  | "repair_rejected" -> Ok Repair_rejected
  | "repair_cancelled" -> Ok Repair_cancelled
  | "repair_expired" -> Ok Repair_expired
  | "repair_stale_revision" -> Ok Repair_stale_revision
  | "auto_link_rejected" -> Ok Auto_link_rejected
  | other -> Error (Printf.sprintf "unknown audit_kind: %s" other)

type redacted_audit_event = {
  version : int;
  id : string;
  kind : audit_kind;
  subject_id : string;
  endpoint_a_key : string;
  endpoint_b_key : string option;
  principal_ids : string list;
  status : string;
  reason : string option;
  timestamp : string;
  details : Yojson.Safe.t;
}

let secret_key_names =
  [
    "proof";
    "proof_secret";
    "secret";
    "token";
    "access_token";
    "refresh_token";
    "password";
    "code";
    "device_code";
    "user_code";
    "pkce";
    "verifier";
    "client_secret";
    "webhook_secret";
    "private_key";
    "authorization";
    "cookie";
  ]

let is_secret_key name =
  let n = String.lowercase_ascii name in
  List.exists
    (fun s -> String.equal n s || String.ends_with ~suffix:s n)
    secret_key_names
  || String.ends_with ~suffix:"_secret" n
  || String.ends_with ~suffix:"_token" n

let rec redact_audit_details = function
  | `Assoc fields ->
      `Assoc
        (List.filter_map
           (fun (k, v) ->
             if is_secret_key k then None
             else if String.equal (String.lowercase_ascii k) "email" then
               (* Contact metadata must not act as a link key in audit exports. *)
               Some (k, `String "[redacted]")
             else Some (k, redact_audit_details v))
           fields)
  | `List xs -> `List (List.map redact_audit_details xs)
  | other -> other

let make_redacted_audit_event ~id ~kind ~subject_id ~endpoint_a_key
    ?endpoint_b_key ?(principal_ids = []) ~status ?reason ?timestamp
    ?(details = `Assoc []) ?now () =
  match require_nonempty "id" id with
  | Error e -> Error e
  | Ok id -> (
      match require_nonempty "subject_id" subject_id with
      | Error e -> Error e
      | Ok subject_id -> (
          match require_nonempty "endpoint_a_key" endpoint_a_key with
          | Error e -> Error e
          | Ok endpoint_a_key ->
              let now =
                match now with Some t -> t | None -> Unix.gettimeofday ()
              in
              let timestamp =
                match timestamp with
                | Some s when String.trim s <> "" -> String.trim s
                | _ -> iso_now ~now ()
              in
              Ok
                {
                  version = protocol_version;
                  id;
                  kind;
                  subject_id;
                  endpoint_a_key;
                  endpoint_b_key;
                  principal_ids;
                  status = String.trim status;
                  reason;
                  timestamp;
                  details = redact_audit_details details;
                }))

let principal_ids_of_endpoints a b =
  List.filter_map
    (fun (e : verified_endpoint) ->
      Option.map P.principal_id_to_string e.principal_id)
    [ a; b ]

let audit_from_link_transaction (tx : link_transaction) ~kind ?id ?reason
    ?(details = `Assoc []) ?now () =
  let now = match now with Some t -> t | None -> Unix.gettimeofday () in
  let id =
    match id with
    | Some s -> s
    | None -> Printf.sprintf "audit_%s_%s" tx.id (string_of_audit_kind kind)
  in
  let details =
    redact_audit_details
      (match details with
      | `Assoc [] ->
          `Assoc
            [
              ("replay_protection_id", `String tx.replay_protection_id);
              ("proof_challenge_id", `String tx.proof_challenge_id);
              ("a_proved", `Bool tx.a_proved);
              ("b_proved", `Bool tx.b_proved);
              ("basis", `String (string_of_link_basis tx.basis));
            ]
      | other -> other)
  in
  {
    version = protocol_version;
    id;
    kind;
    subject_id = tx.id;
    endpoint_a_key = P.actor_identity_key tx.endpoint_a.actor_key;
    endpoint_b_key = Some (P.actor_identity_key tx.endpoint_b.actor_key);
    principal_ids = principal_ids_of_endpoints tx.endpoint_a tx.endpoint_b;
    status = string_of_link_tx_status tx.status;
    reason;
    timestamp = iso_now ~now ();
    details;
  }

let audit_from_repair_plan (plan : admin_repair_plan) ~kind ?id ?reason
    ?(details = `Assoc []) ?now () =
  let now = match now with Some t -> t | None -> Unix.gettimeofday () in
  let id =
    match id with
    | Some s -> s
    | None -> Printf.sprintf "audit_%s_%s" plan.id (string_of_audit_kind kind)
  in
  let details =
    redact_audit_details
      (match details with
      | `Assoc [] ->
          `Assoc
            [
              ("digest", `String plan.digest);
              ("basis", `String (string_of_link_basis plan.basis));
              ( "admin_principal_id",
                `String (P.principal_id_to_string plan.admin_principal_id) );
            ]
      | other -> other)
  in
  {
    version = protocol_version;
    id;
    kind;
    subject_id = plan.id;
    endpoint_a_key = P.actor_identity_key plan.endpoint_a.actor_key;
    endpoint_b_key = Some (P.actor_identity_key plan.endpoint_b.actor_key);
    principal_ids =
      P.principal_id_to_string plan.admin_principal_id
      :: principal_ids_of_endpoints plan.endpoint_a plan.endpoint_b;
    status = string_of_repair_status plan.status;
    reason;
    timestamp = iso_now ~now ();
    details;
  }

let rec json_has_secret_keys = function
  | `Assoc fields ->
      List.exists
        (fun (k, v) ->
          is_secret_key k
          || (String.equal (String.lowercase_ascii k) "email"
             &&
             match v with
             | `String s -> not (String.equal s "[redacted]")
             | _ -> true)
          || json_has_secret_keys v)
        fields
  | `List xs -> List.exists json_has_secret_keys xs
  | _ -> false

let audit_event_is_redacted (e : redacted_audit_event) =
  let cleaned = redact_audit_details e.details in
  Yojson.Safe.equal cleaned e.details && not (json_has_secret_keys e.details)

let redacted_audit_event_to_json (e : redacted_audit_event) =
  let details = redact_audit_details e.details in
  `Assoc
    [
      ("version", `Int e.version);
      ("id", `String e.id);
      ("kind", `String (string_of_audit_kind e.kind));
      ("subject_id", `String e.subject_id);
      ("endpoint_a_key", `String e.endpoint_a_key);
      ( "endpoint_b_key",
        match e.endpoint_b_key with None -> `Null | Some k -> `String k );
      ("principal_ids", `List (List.map (fun s -> `String s) e.principal_ids));
      ("status", `String e.status);
      ("reason", match e.reason with None -> `Null | Some r -> `String r);
      ("timestamp", `String e.timestamp);
      ("details", details);
    ]

let opt_delivery_json = function
  | None -> `Null
  | Some d -> private_proof_delivery_to_json d

let link_transaction_to_json (tx : link_transaction) =
  `Assoc
    [
      ("version", `Int tx.version);
      ("id", `String tx.id);
      ("basis", `String (string_of_link_basis tx.basis));
      ("endpoint_a", verified_endpoint_to_json tx.endpoint_a);
      ("endpoint_b", verified_endpoint_to_json tx.endpoint_b);
      ("initiator", `String (side_to_string tx.initiator));
      ("status", `String (string_of_link_tx_status tx.status));
      ("replay_protection_id", `String tx.replay_protection_id);
      ("proof_challenge_id", `String tx.proof_challenge_id);
      ("a_proved", `Bool tx.a_proved);
      ("b_proved", `Bool tx.b_proved);
      ("delivery_a", opt_delivery_json tx.delivery_a);
      ("delivery_b", opt_delivery_json tx.delivery_b);
      ("created_at", `String tx.created_at);
      ("expires_at", `String tx.expires_at);
      ( "completed_at",
        match tx.completed_at with None -> `Null | Some s -> `String s );
      ( "cancelled_at",
        match tx.cancelled_at with None -> `Null | Some s -> `String s );
      ( "cancel_reason",
        match tx.cancel_reason with None -> `Null | Some s -> `String s );
    ]

let admin_repair_plan_to_json (plan : admin_repair_plan) =
  `Assoc
    [
      ("version", `Int plan.version);
      ("id", `String plan.id);
      ("basis", `String (string_of_link_basis plan.basis));
      ("endpoint_a", verified_endpoint_to_json plan.endpoint_a);
      ("endpoint_b", verified_endpoint_to_json plan.endpoint_b);
      ( "admin_principal_id",
        `String (P.principal_id_to_string plan.admin_principal_id) );
      ("survivor", survivor_to_json plan.survivor);
      ( "base_principal_a_revision",
        match plan.base_principal_a_revision with
        | None -> `Null
        | Some r -> `Int r );
      ( "base_principal_b_revision",
        match plan.base_principal_b_revision with
        | None -> `Null
        | Some r -> `Int r );
      ("preview", preview_to_json plan.preview);
      ("digest", `String plan.digest);
      ("status", `String (string_of_repair_status plan.status));
      ("created_at", `String plan.created_at);
      ("expires_at", `String plan.expires_at);
      ( "confirmed_at",
        match plan.confirmed_at with None -> `Null | Some s -> `String s );
      ( "applied_at",
        match plan.applied_at with None -> `Null | Some s -> `String s );
      ( "reject_reason",
        match plan.reject_reason with None -> `Null | Some s -> `String s );
    ]
