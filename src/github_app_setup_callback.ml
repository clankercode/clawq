(* Verify and exchange GitHub App manifest browser callbacks (P19.M2.E1.T002).
   See github_app_setup_callback.mli and
   docs/plans/2026-07-12-github-item-room-routing.md. *)

type exchange_request = {
  code : string;
  state : string;
  callback_path : string option;
  expected_bind : Github_app_setup_tx.bind_target option;
  expected_principal_id : string option;
  installation_id : int option;
  setup_action : string option;
}

type app_credentials = {
  app_id : int;
  slug : string option;
  client_id_handle : string;
  client_secret_handle : string;
  private_key_handle : string;
  webhook_secret_handle : string;
  html_url : string option;
  owner : string option;
}

type exchange_result = {
  transaction : Github_app_setup_tx.t;
  app : app_credentials;
  installation_id : int option;
  raw_app_id : int;
  receipt_id : string;
}

type http_post =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, string) result

type store_secret = name:string -> plaintext:string -> (string, string) result

let conversion_url ~code =
  Printf.sprintf "https://api.github.com/app-manifests/%s/conversions"
    (Uri.pct_encode ~component:`Path code)

let trim_trailing_slash s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '/' then String.sub s 0 (len - 1) else s

let join_url base path =
  let base = trim_trailing_slash base in
  let path =
    if String.length path > 0 && path.[0] = '/' then path else "/" ^ path
  in
  base ^ path

let expected_callback_url ~public_base_url =
  join_url public_base_url Github_app_setup_tx.default_callback_path

let normalize_callback_ref ~public_base_url ref_s =
  let s = String.trim ref_s in
  if s = "" then Error "callback_path is empty"
  else
    let lower = String.lowercase_ascii s in
    let is_absolute =
      String.starts_with ~prefix:"http://" lower
      || String.starts_with ~prefix:"https://" lower
    in
    if is_absolute then Ok (trim_trailing_slash s)
    else
      let path = if s.[0] = '/' then s else "/" ^ s in
      Ok (join_url public_base_url path)

let truncate_body ~max_len body =
  if String.length body <= max_len then body
  else String.sub body 0 max_len ^ "..."

let ensure_schema db =
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_app_setup_exchange (
      id TEXT PRIMARY KEY NOT NULL,
      tx_id TEXT NOT NULL UNIQUE,
      app_id INTEGER NOT NULL,
      slug TEXT,
      client_id_handle TEXT NOT NULL,
      client_secret_handle TEXT NOT NULL,
      private_key_handle TEXT NOT NULL,
      webhook_secret_handle TEXT NOT NULL,
      html_url TEXT,
      owner TEXT,
      installation_id INTEGER,
      created_at TEXT NOT NULL
    )|}
  in
  let idx =
    {|CREATE INDEX IF NOT EXISTS idx_github_app_setup_exchange_tx
      ON github_app_setup_exchange(tx_id)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "github_app_setup_callback schema error: %s"
               (Sqlite3.Rc.to_string rc)))
    [ table_sql; idx ]

let generate_receipt_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghapp_ex_%d_%06d" ts rand

let default_store_secret ~name ~plaintext =
  if String.trim plaintext = "" then
    Error (Printf.sprintf "refusing to store empty secret: %s" name)
  else
    match Secret_store.get_master_key () with
    | Error msg ->
        Error
          (Printf.sprintf
             "credential store unavailable for %s (set CLAWQ_MASTER_KEY or \
              inject store_secret): %s"
             name msg)
    | Ok key -> Ok (Secret_store.encrypt_secret ~key plaintext)

let conversion_headers =
  [
    ("Accept", "application/vnd.github+json");
    ("User-Agent", "clawq-github-app-setup");
    ("X-GitHub-Api-Version", "2022-11-28");
  ]

type conversion_payload = {
  app_id : int;
  slug : string option;
  client_id : string;
  client_secret : string;
  pem : string;
  webhook_secret : string;
  html_url : string option;
  owner : string option;
}

let json_string_field j name =
  match Yojson.Safe.Util.member name j with
  | `String s when String.trim s <> "" -> Ok s
  | `String _ -> Error (Printf.sprintf "conversion response %s is empty" name)
  | `Null -> Error (Printf.sprintf "conversion response missing %s" name)
  | _ -> Error (Printf.sprintf "conversion response %s must be a string" name)

let json_opt_string j name =
  match Yojson.Safe.Util.member name j with
  | `String s when String.trim s <> "" -> Some s
  | _ -> None

let json_int_field j name =
  match Yojson.Safe.Util.member name j with
  | `Int i -> Ok i
  | `Intlit s -> (
      try Ok (int_of_string s)
      with Failure _ ->
        Error (Printf.sprintf "conversion response %s is not an int" name))
  | `Null -> Error (Printf.sprintf "conversion response missing %s" name)
  | _ -> Error (Printf.sprintf "conversion response %s must be an integer" name)

let owner_login j =
  match Yojson.Safe.Util.member "owner" j with
  | `Assoc _ as o -> (
      match Yojson.Safe.Util.member "login" o with
      | `String s when String.trim s <> "" -> Some s
      | _ -> (
          match Yojson.Safe.Util.member "slug" o with
          | `String s when String.trim s <> "" -> Some s
          | _ -> (
              match Yojson.Safe.Util.member "name" o with
              | `String s when String.trim s <> "" -> Some s
              | _ -> None)))
  | _ -> None

let parse_conversion_body body =
  try
    let j = Yojson.Safe.from_string body in
    match json_int_field j "id" with
    | Error e -> Error e
    | Ok app_id -> (
        match json_string_field j "client_id" with
        | Error e -> Error e
        | Ok client_id -> (
            match json_string_field j "client_secret" with
            | Error e -> Error e
            | Ok client_secret -> (
                match json_string_field j "pem" with
                | Error e -> Error e
                | Ok pem -> (
                    match json_string_field j "webhook_secret" with
                    | Error e -> Error e
                    | Ok webhook_secret ->
                        Ok
                          {
                            app_id;
                            slug = json_opt_string j "slug";
                            client_id;
                            client_secret;
                            pem;
                            webhook_secret;
                            html_url = json_opt_string j "html_url";
                            owner = owner_login j;
                          }))))
  with Yojson.Json_error msg ->
    Error (Printf.sprintf "malformed conversion JSON: %s" msg)

let store_all_secrets ~(store_secret : store_secret)
    ~(payload : conversion_payload) ~tx_id =
  (* Complete secret set is validated by parse_conversion_body first. Store all
     four required secrets; any failure leaves the transaction open and does not
     apply Runtime_config. *)
  let name kind = Printf.sprintf "github_app.%s.%s" tx_id kind in
  match store_secret ~name:(name "client_id") ~plaintext:payload.client_id with
  | Error e -> Error e
  | Ok client_id_handle -> (
      match
        store_secret ~name:(name "client_secret")
          ~plaintext:payload.client_secret
      with
      | Error e -> Error e
      | Ok client_secret_handle -> (
          match
            store_secret ~name:(name "private_key") ~plaintext:payload.pem
          with
          | Error e -> Error e
          | Ok private_key_handle -> (
              match
                store_secret ~name:(name "webhook_secret")
                  ~plaintext:payload.webhook_secret
              with
              | Error e -> Error e
              | Ok webhook_secret_handle ->
                  Ok
                    {
                      app_id = payload.app_id;
                      slug = payload.slug;
                      client_id_handle;
                      client_secret_handle;
                      private_key_handle;
                      webhook_secret_handle;
                      html_url = payload.html_url;
                      owner = payload.owner;
                    })))

let insert_receipt ~db ~id ~tx_id ~(app : app_credentials) ~installation_id
    ~created_at =
  let sql =
    {|INSERT INTO github_app_setup_exchange
      (id, tx_id, app_id, slug, client_id_handle, client_secret_handle,
       private_key_handle, webhook_secret_handle, html_url, owner,
       installation_id, created_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i v = ignore (Sqlite3.bind stmt i v) in
  bind 1 (Sqlite3.Data.TEXT id);
  bind 2 (Sqlite3.Data.TEXT tx_id);
  bind 3 (Sqlite3.Data.INT (Int64.of_int app.app_id));
  bind 4
    (match app.slug with
    | None -> Sqlite3.Data.NULL
    | Some s -> Sqlite3.Data.TEXT s);
  bind 5 (Sqlite3.Data.TEXT app.client_id_handle);
  bind 6 (Sqlite3.Data.TEXT app.client_secret_handle);
  bind 7 (Sqlite3.Data.TEXT app.private_key_handle);
  bind 8 (Sqlite3.Data.TEXT app.webhook_secret_handle);
  bind 9
    (match app.html_url with
    | None -> Sqlite3.Data.NULL
    | Some s -> Sqlite3.Data.TEXT s);
  bind 10
    (match app.owner with
    | None -> Sqlite3.Data.NULL
    | Some s -> Sqlite3.Data.TEXT s);
  bind 11
    (match installation_id with
    | None -> Sqlite3.Data.NULL
    | Some i -> Sqlite3.Data.INT (Int64.of_int i));
  bind 12 (Sqlite3.Data.TEXT created_at);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_app_setup_exchange insert failed: %s"
           (Sqlite3.Rc.to_string rc))

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT i -> Int64.to_int i
  | Sqlite3.Data.TEXT s -> ( try int_of_string s with _ -> 0)
  | _ -> 0

let row_to_app stmt =
  {
    app_id = int_col stmt 0;
    slug = opt_text_col stmt 1;
    client_id_handle = text_col stmt 2;
    client_secret_handle = text_col stmt 3;
    private_key_handle = text_col stmt 4;
    webhook_secret_handle = text_col stmt 5;
    html_url = opt_text_col stmt 6;
    owner = opt_text_col stmt 7;
  }

let get_receipt ~db ~id =
  let sql =
    {|SELECT app_id, slug, client_id_handle, client_secret_handle,
             private_key_handle, webhook_secret_handle, html_url, owner
      FROM github_app_setup_exchange WHERE id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> Ok (Some (row_to_app stmt))
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "github_app_setup_exchange get failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let find_receipt_by_tx ~db ~tx_id =
  let sql =
    {|SELECT id, app_id, slug, client_id_handle, client_secret_handle,
             private_key_handle, webhook_secret_handle, html_url, owner
      FROM github_app_setup_exchange WHERE tx_id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT tx_id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let rid = text_col stmt 0 in
        let app =
          {
            app_id = int_col stmt 1;
            slug = opt_text_col stmt 2;
            client_id_handle = text_col stmt 3;
            client_secret_handle = text_col stmt 4;
            private_key_handle = text_col stmt 5;
            webhook_secret_handle = text_col stmt 6;
            html_url = opt_text_col stmt 7;
            owner = opt_text_col stmt 8;
          }
        in
        Ok (Some (rid, app))
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "github_app_setup_exchange find_by_tx failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let mark_expired ~db ~id ~now =
  let updated_at = Time_util.iso8601_utc ~t:now () in
  let sql =
    {|UPDATE github_app_setup_tx
      SET status = 'expired', updated_at = ?
      WHERE id = ? AND status = 'open'|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT updated_at));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT id));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let validate_transaction ~db ~(req : exchange_request) ~now =
  if String.trim req.code = "" then Error "code is required"
  else if String.trim req.state = "" then Error "state is required"
  else
    match Github_app_setup_tx.find_by_state ~db ~state:req.state with
    | Error e -> Error e
    | Ok None -> Error "unknown setup state: no matching transaction"
    | Ok (Some tx) -> (
        if tx.status <> Github_app_setup_tx.Open then
          Error
            (Printf.sprintf
               "setup transaction is not open (status=%s); refusing reuse"
               (Github_app_setup_tx.status_to_string tx.status))
        else if Github_app_setup_tx.is_expired ~now tx then (
          mark_expired ~db ~id:tx.id ~now;
          Error "setup transaction expired")
        else
          let bind_ok =
            match req.expected_bind with
            | None -> true
            | Some expected ->
                Github_app_setup_tx.bind_to_string expected
                = Github_app_setup_tx.bind_to_string tx.bind
          in
          if not bind_ok then
            let expected =
              match req.expected_bind with
              | Some b -> Github_app_setup_tx.bind_to_string b
              | None -> "?"
            in
            Error
              (Printf.sprintf
                 "bind target mismatch: callback expected %s but transaction \
                  is bound to %s"
                 expected
                 (Github_app_setup_tx.bind_to_string tx.bind))
          else
            let principal_ok =
              match req.expected_principal_id with
              | None -> true
              | Some pid -> pid = tx.principal.id
            in
            if not principal_ok then
              let expected =
                match req.expected_principal_id with Some p -> p | None -> "?"
              in
              Error
                (Printf.sprintf
                   "principal mismatch: callback expected %s but transaction \
                    belongs to %s"
                   expected tx.principal.id)
            else
              match req.callback_path with
              | None -> Ok tx
              | Some path_ref -> (
                  match
                    normalize_callback_ref ~public_base_url:tx.public_base_url
                      path_ref
                  with
                  | Error e -> Error e
                  | Ok got ->
                      let expected =
                        expected_callback_url
                          ~public_base_url:tx.public_base_url
                      in
                      if trim_trailing_slash got <> trim_trailing_slash expected
                      then
                        Error
                          (Printf.sprintf
                             "callback path mismatch: got %s expected %s" got
                             expected)
                      else Ok tx))

let exchange ~db ?http_post ?(store_secret = default_store_secret)
    ?(now = Unix.gettimeofday ()) (req : exchange_request) =
  ensure_schema db;
  match validate_transaction ~db ~req ~now with
  | Error e -> Error e
  | Ok tx -> (
      let http =
        match http_post with
        | Some f -> f
        | None ->
            fun ~url:_ ~headers:_ ~body:_ ->
              Error
                "http_post not provided: inject a client or wire production \
                 HTTP"
      in
      let url = conversion_url ~code:req.code in
      match http ~url ~headers:conversion_headers ~body:"" with
      | Error e ->
          Error (Printf.sprintf "GitHub conversion HTTP transport error: %s" e)
      | Ok (status, body) -> (
          if status < 200 || status >= 300 then
            Error
              (Printf.sprintf "GitHub conversion HTTP error: status=%d body=%s"
                 status
                 (truncate_body ~max_len:200 body))
          else
            match parse_conversion_body body with
            | Error e -> Error e
            | Ok payload -> (
                match store_all_secrets ~store_secret ~payload ~tx_id:tx.id with
                | Error e ->
                    Error
                      (Printf.sprintf
                         "credential store failed (transaction left open, no \
                          partial config): %s"
                         e)
                | Ok app -> (
                    let receipt_id = generate_receipt_id ~now () in
                    let created_at = Time_util.iso8601_utc ~t:now () in
                    (* Consume only after the full secret set is stored. Atomic
                       consume rejects concurrent duplicates. No Runtime_config
                       mutation — handles only. *)
                    match
                      Github_app_setup_tx.mark_consumed ~db ~id:tx.id
                        ~principal_id:tx.principal.id ~now ()
                    with
                    | Error e ->
                        Error
                          (Printf.sprintf
                             "failed to mark transaction consumed after \
                              exchange (secrets stored as handles only; no \
                              Runtime_config applied): %s"
                             e)
                    | Ok consumed ->
                        let _ =
                          insert_receipt ~db ~id:receipt_id ~tx_id:tx.id ~app
                            ~installation_id:req.installation_id ~created_at
                        in
                        Ok
                          {
                            transaction = consumed;
                            app;
                            installation_id = req.installation_id;
                            raw_app_id = app.app_id;
                            receipt_id;
                          }))))
