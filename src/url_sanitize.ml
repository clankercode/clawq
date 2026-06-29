(** URL sanitization for safe display in room messages.

    Strips sensitive parameters (tokens, API keys, passwords, secrets) from URLs
    before displaying them in Teams/Slack messages. This prevents accidental
    secret leakage through progress updates and task links.

    The module handles common patterns:
    - Query parameters with sensitive names (token, key, secret, password, etc.)
    - Bearer tokens in URL fragments
    - Credentials in URL userinfo (user:pass@host) *)

(** {1 Sensitive parameter detection} *)

(** List of query parameter names that are considered sensitive.
    Case-insensitive matching is used. *)
let sensitive_param_names =
  [
    "token";
    "access_token";
    "refresh_token";
    "api_key";
    "apikey";
    "api-key";
    "key";
    "secret";
    "client_secret";
    "client_id";
    "password";
    "passwd";
    "pwd";
    "auth";
    "authorization";
    "bearer";
    "credential";
    "credentials";
    "private_key";
    "private-key";
    "signing_key";
    "signing-key";
    "webhook_secret";
    "webhook-secret";
  ]

(** [is_sensitive_param name] returns [true] if the query parameter name matches
    a known sensitive pattern. Uses case-insensitive comparison and also checks
    for suffixes like [_token], [_secret], [_key]. *)
let is_sensitive_param name =
  let lower = String.lowercase_ascii name in
  let contains sub s =
    let sub_len = String.length sub in
    let s_len = String.length s in
    if sub_len > s_len then false
    else
      let rec search i =
        if i + sub_len > s_len then false
        else if String.sub s i sub_len = sub then true
        else search (i + 1)
      in
      search 0
  in
  List.exists
    (fun sensitive -> String.equal lower sensitive)
    sensitive_param_names
  || contains "_token" lower || contains "_secret" lower
  || contains "_key" lower || contains "_password" lower

(** {1 URL sanitization} *)

(** [mask_value _value] returns a masked representation of a sensitive value.
    Shows only the first 4 characters followed by "***". *)
let mask_value value =
  let len = String.length value in
  if len <= 4 then "***" else String.sub value 0 4 ^ "***"

(** [sanitize_query_params query] masks sensitive query parameters. *)
let sanitize_query_params query =
  List.map
    (fun (name, values) ->
      if is_sensitive_param name then
        let masked_values = List.map (fun v -> mask_value v) values in
        (name, masked_values)
      else (name, values))
    query

(** [mask_userinfo uri] masks credentials in the userinfo portion of a URI.
    Handles both [user:pass@host] and [token@host] patterns. *)
let mask_userinfo uri =
  match Uri.userinfo uri with
  | Some _userinfo ->
      (* Mask entire userinfo to prevent token leakage *)
      Uri.with_userinfo uri (Some "REDACTED")
  | None -> uri

(** [sanitize_url url] is the main entry point for URL sanitization. Strips
    sensitive query parameters and masks credentials in userinfo.

    Examples:
    - [sanitize_url "https://example.com/page?token=abc123&name=foo"] returns
      ["https://example.com/page?token=abc***&name=foo"]
    - [sanitize_url "https://user:password@host/path"] returns
      ["https://user:***@host/path"] *)
let sanitize_url (url : string) =
  let trimmed = String.trim url in
  if trimmed = "" then ""
  else
    try
      let uri = Uri.of_string trimmed in
      (* Mask userinfo if present *)
      let uri = mask_userinfo uri in
      (* Sanitize query parameters *)
      let query = Uri.query uri in
      let sanitized = sanitize_query_params query in
      let clean_uri = Uri.with_query uri sanitized in
      (* Remove sensitive fragments *)
      let clean_uri =
        match Uri.fragment clean_uri with
        | Some frag ->
            let lower_frag = String.lowercase_ascii frag in
            let contains sub s =
              let sub_len = String.length sub in
              let s_len = String.length s in
              if sub_len > s_len then false
              else
                let rec search i =
                  if i + sub_len > s_len then false
                  else if String.sub s i sub_len = sub then true
                  else search (i + 1)
                in
                search 0
            in
            if
              contains "token" lower_frag
              || contains "secret" lower_frag
              || contains "key" lower_frag
            then Uri.with_fragment clean_uri None
            else clean_uri
        | None -> clean_uri
      in
      Uri.to_string clean_uri
    with _ ->
      (* If URI parsing fails, return a safe placeholder rather than
         potentially unsanitized URL *)
      "[invalid-url]"

(** {1 Safe link formatting} *)

(** [safe_teams_link url label] creates a Teams-formatted markdown link with
    sanitized URL. Returns a string like ["[label](sanitized_url)"]. *)
let safe_teams_link url label =
  Printf.sprintf "[%s](%s)" label (sanitize_url url)

(** [safe_slack_link url label] creates a Slack mrkdwn-formatted link with
    sanitized URL. Returns a string like ["<sanitized_url|label>"]. *)
let safe_slack_link url label =
  Printf.sprintf "<%s|%s>" (sanitize_url url) label
