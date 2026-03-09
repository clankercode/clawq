(* Provider quota state fetching and caching for quota-aware routing.
   Supports: Anthropic, Codex/OpenAI, Z.ai, Kimi, Cursor.
   Unknown providers are never considered constrained (soft preference only). *)

type window_state = {
  used_pct : float;
  resets_at : float option;
  window_duration_s : float option;
}

type quota_state =
  | Known of {
      session : window_state option;
      weekly : window_state option;
      monthly : window_state option;
    }
  | Unknown of string

type provider_quota = {
  provider_name : string;
  state : quota_state;
  fetched_at : float;
}

(* In-memory TTL cache: provider name -> latest quota *)
let cache : (string, provider_quota) Hashtbl.t = Hashtbl.create 8
let cache_ttl_s = ref 300.0
let set_cache_ttl s = cache_ttl_s := float_of_int s

let get_cached name =
  match Hashtbl.find_opt cache name with
  | None -> None
  | Some pq ->
      let age = Unix.gettimeofday () -. pq.fetched_at in
      if age < !cache_ttl_s then Some pq else None

let get_all_cached () =
  let now = Unix.gettimeofday () in
  Hashtbl.fold
    (fun _name pq acc ->
      let age = now -. pq.fetched_at in
      if age < !cache_ttl_s then (pq.provider_name, pq) :: acc else acc)
    cache []

let store_result pq =
  match pq.state with
  | Known _ -> Hashtbl.replace cache pq.provider_name pq
  | Unknown s when String.length s >= 12 && String.sub s 0 12 = "fetch_failed"
    ->
      (* On fetch failure, keep existing stale entry if present *)
      if not (Hashtbl.mem cache pq.provider_name) then
        Hashtbl.replace cache pq.provider_name pq
  | Unknown _ -> Hashtbl.replace cache pq.provider_name pq

(* Pace-aware constrained check for a single window *)
let is_window_constrained ~threshold w =
  let u = w.used_pct /. 100.0 in
  if u >= threshold then true
  else
    match (w.resets_at, w.window_duration_s) with
    | Some ts, Some dur ->
        let now = Unix.gettimeofday () in
        let window_start = ts -. dur in
        let elapsed = now -. window_start in
        if elapsed <= 0.0 || dur <= 0.0 then false
        else
          let time_elapsed_pct = elapsed /. dur *. 100.0 in
          if time_elapsed_pct <= 0.0 then false
          else
            let pace_ratio = w.used_pct /. time_elapsed_pct in
            pace_ratio >= 1.5 && w.used_pct >= 50.0
    | _ -> false

(* Returns true when a provider should be deprioritised due to quota pressure.
   Unknown providers are never considered constrained. *)
let is_constrained ?(threshold = 0.85) state =
  match state with
  | Unknown _ -> false
  | Known { session; weekly; monthly } ->
      let check = function
        | None -> false
        | Some w -> is_window_constrained ~threshold w
      in
      check session || check weekly || check monthly

let format_time_remaining_s s =
  let s = int_of_float s in
  if s < 60 then Printf.sprintf "%ds" s
  else if s < 3600 then Printf.sprintf "%dm" (s / 60)
  else if s < 86400 then Printf.sprintf "%dh%02dm" (s / 3600) (s mod 3600 / 60)
  else Printf.sprintf "%dd%dh" (s / 86400) (s mod 86400 / 3600)

let window_to_string label window =
  match window with
  | None -> ""
  | Some { used_pct; resets_at; _ } ->
      let remaining =
        match resets_at with
        | None -> ""
        | Some ts ->
            let r = ts -. Unix.gettimeofday () in
            if r > 0.0 then
              Printf.sprintf " (%s left)" (format_time_remaining_s r)
            else ""
      in
      Printf.sprintf "%s=%.0f%%%s" label used_pct remaining

let to_summary_string pq =
  match pq.state with
  | Unknown s -> Printf.sprintf "%s\tUnknown (%s)" pq.provider_name s
  | Known { session; weekly; monthly } ->
      let parts =
        List.filter_map
          (fun s -> if s = "" then None else Some s)
          [
            window_to_string "session" session;
            window_to_string "weekly" weekly;
            window_to_string "monthly" monthly;
          ]
      in
      Printf.sprintf "%s\t%s" pq.provider_name (String.concat "  " parts)

(* Build a notice string to inject into system prompt when provider is >= threshold
   usage. Returns None when not applicable. *)
let quota_notice ?(threshold = 0.70) pq =
  match pq.state with
  | Unknown _ -> None
  | Known { session; weekly; monthly } ->
      let any_triggered = function
        | None -> false
        | Some { used_pct; _ } -> used_pct /. 100.0 >= threshold
      in
      if any_triggered session || any_triggered weekly || any_triggered monthly
      then
        let parts =
          List.filter_map
            (fun (label, w) ->
              match w with
              | Some { used_pct; resets_at; _ }
                when used_pct /. 100.0 >= threshold ->
                  let remaining =
                    match resets_at with
                    | None -> ""
                    | Some ts ->
                        let r = ts -. Unix.gettimeofday () in
                        if r > 0.0 then
                          Printf.sprintf " (%s left)"
                            (format_time_remaining_s r)
                        else ""
                  in
                  Some (Printf.sprintf "%s %.0f%%%s" label used_pct remaining)
              | _ -> None)
            [ ("session", session); ("weekly", weekly); ("monthly", monthly) ]
        in
        Some
          (Printf.sprintf "[quota] %s: %s — prefer concise responses"
             pq.provider_name (String.concat ", " parts))
      else None

(* Returns the status label for CLI display *)
let status_label ?(threshold = 0.85) pq =
  match pq.state with
  | Unknown _ -> "unknown"
  | Known _ ->
      if is_constrained ~threshold pq.state then "constrained" else "ok"

(* ── Credential and HTTP helpers ─────────────────────────────────────────── *)

let read_file_opt path =
  try
    let ic = open_in path in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some s
  with _ -> None

let float_member json key =
  try Some Yojson.Safe.Util.(json |> member key |> to_float)
  with _ -> (
    try Some (float_of_int Yojson.Safe.Util.(json |> member key |> to_int))
    with _ -> None)

let string_member json key =
  try Some Yojson.Safe.Util.(json |> member key |> to_string) with _ -> None

(* Parse ISO 8601 UTC timestamp to unix epoch float.
   Handles "2024-01-15T10:00:00Z" and "2024-01-15T10:00:00.000Z". *)
let parse_iso8601 s =
  try
    let s = String.trim s in
    let len = String.length s in
    let s =
      if len > 0 && s.[len - 1] = 'Z' then String.sub s 0 (len - 1) else s
    in
    let date_part, time_part =
      match String.split_on_char 'T' s with
      | [ d; t ] -> (d, t)
      | _ -> failwith "no T separator"
    in
    let year, month, day =
      match String.split_on_char '-' date_part with
      | [ y; m; d ] -> (int_of_string y, int_of_string m, int_of_string d)
      | _ -> failwith "bad date part"
    in
    let time_part =
      match String.split_on_char '.' time_part with
      | [ t; _ ] -> t
      | _ -> time_part
    in
    let hour, min, sec =
      match String.split_on_char ':' time_part with
      | [ h; m; s ] -> (int_of_string h, int_of_string m, int_of_string s)
      | _ -> failwith "bad time part"
    in
    let tm =
      {
        Unix.tm_sec = sec;
        tm_min = min;
        tm_hour = hour;
        tm_mday = day;
        tm_mon = month - 1;
        tm_year = year - 1900;
        tm_wday = 0;
        tm_yday = 0;
        tm_isdst = false;
      }
    in
    (* Convert UTC struct tm to unix epoch via mktime (local) then adjust
       for the local timezone offset. *)
    let local_ts, _ = Unix.mktime tm in
    let dummy_gm = Unix.gmtime 0.0 in
    let dummy_local = Unix.localtime 0.0 in
    let tz_offset_s =
      float_of_int
        (((dummy_local.Unix.tm_hour - dummy_gm.Unix.tm_hour) * 3600)
        + ((dummy_local.Unix.tm_min - dummy_gm.Unix.tm_min) * 60))
    in
    Some (local_ts -. tz_offset_s)
  with _ -> None

(* ── Per-provider fetch functions ────────────────────────────────────────── *)

let make_unknown provider_name msg =
  let pq =
    { provider_name; state = Unknown msg; fetched_at = Unix.gettimeofday () }
  in
  store_result pq;
  Lwt.return pq

(* Anthropic quota via OAuth usage API.
   Token source: ~/.claude/.credentials.json -> .claudeAiOauth.accessToken
   Required header: anthropic-beta: oauth-2025-04-20 *)
let fetch_anthropic ~credentials_file () =
  let open Lwt.Syntax in
  let provider_name = "anthropic" in
  match read_file_opt credentials_file with
  | None -> make_unknown provider_name "not_configured"
  | Some contents -> (
      try
        let json = Yojson.Safe.from_string contents in
        let token =
          Yojson.Safe.Util.(
            json |> member "claudeAiOauth" |> member "accessToken" |> to_string)
        in
        let uri = "https://api.anthropic.com/api/oauth/usage" in
        let headers =
          [
            ("Authorization", "Bearer " ^ token);
            ("anthropic-beta", "oauth-2025-04-20");
          ]
        in
        Lwt.catch
          (fun () ->
            let* status, body = Http_client.get ~uri ~headers in
            if status < 200 || status >= 300 then
              make_unknown provider_name
                (Printf.sprintf "fetch_failed:HTTP %d" status)
            else
              let j = Yojson.Safe.from_string body in
              let parse_window key dur_s =
                try
                  let w = Yojson.Safe.Util.(j |> member key) in
                  let utilization =
                    match float_member w "utilization" with
                    | Some u -> u
                    | None -> failwith "no utilization"
                  in
                  let resets_at =
                    match string_member w "resets_at" with
                    | Some s -> parse_iso8601 s
                    | None -> float_member w "reset_at"
                  in
                  Some
                    {
                      used_pct = utilization *. 100.0;
                      resets_at;
                      window_duration_s = Some dur_s;
                    }
                with _ -> None
              in
              let session = parse_window "five_hour" 18000.0 in
              let weekly = parse_window "seven_day" 604800.0 in
              let pq =
                {
                  provider_name;
                  state = Known { session; weekly; monthly = None };
                  fetched_at = Unix.gettimeofday ();
                }
              in
              store_result pq;
              Lwt.return pq)
          (fun exn ->
            make_unknown provider_name
              (Printf.sprintf "fetch_failed:%s" (Printexc.to_string exn)))
      with _ -> make_unknown provider_name "not_configured")

(* Codex/OpenAI quota via wham usage API.
   Token source: ~/.codex/auth.json -> .tokens.access_token *)
let fetch_codex ~credentials_file ?account_id () =
  let open Lwt.Syntax in
  let provider_name = "codex" in
  match read_file_opt credentials_file with
  | None -> make_unknown provider_name "not_configured"
  | Some contents -> (
      try
        let json = Yojson.Safe.from_string contents in
        let token =
          Yojson.Safe.Util.(
            json |> member "tokens" |> member "access_token" |> to_string)
        in
        let uri = "https://chatgpt.com/backend-api/wham/usage" in
        let headers =
          [ ("Authorization", "Bearer " ^ token) ]
          @
          match account_id with
          | Some id -> [ ("ChatGPT-Account-Id", id) ]
          | None -> []
        in
        Lwt.catch
          (fun () ->
            let* status, body = Http_client.get ~uri ~headers in
            if status < 200 || status >= 300 then
              make_unknown provider_name
                (Printf.sprintf "fetch_failed:HTTP %d" status)
            else
              let j = Yojson.Safe.from_string body in
              let parse_window key dur_s =
                try
                  let w =
                    Yojson.Safe.Util.(j |> member "rate_limit" |> member key)
                  in
                  let used_pct =
                    match float_member w "used_percent" with
                    | Some p -> p
                    | None -> failwith "no used_percent"
                  in
                  let resets_at = float_member w "reset_at" in
                  Some { used_pct; resets_at; window_duration_s = Some dur_s }
                with _ -> None
              in
              let session = parse_window "primary_window" 18000.0 in
              let weekly = parse_window "secondary_window" 604800.0 in
              let pq =
                {
                  provider_name;
                  state = Known { session; weekly; monthly = None };
                  fetched_at = Unix.gettimeofday ();
                }
              in
              store_result pq;
              Lwt.return pq)
          (fun exn ->
            make_unknown provider_name
              (Printf.sprintf "fetch_failed:%s" (Printexc.to_string exn)))
      with _ -> make_unknown provider_name "not_configured")

(* Z.ai quota via monitor API.
   Uses raw api_key — NO "Bearer " prefix. *)
let fetch_zai ~api_key () =
  let open Lwt.Syntax in
  let provider_name = "zai" in
  if api_key = "" then make_unknown provider_name "not_configured"
  else
    let uri = "https://api.z.ai/api/monitor/usage/quota/limit" in
    let headers = [ ("Authorization", api_key) ] in
    Lwt.catch
      (fun () ->
        let* status, body = Http_client.get ~uri ~headers in
        if status < 200 || status >= 300 then
          make_unknown provider_name
            (Printf.sprintf "fetch_failed:HTTP %d" status)
        else
          let j = Yojson.Safe.from_string body in
          let limits =
            try
              Yojson.Safe.Util.(
                j |> member "data" |> member "limits" |> to_list)
            with _ -> []
          in
          let find_limit type_str =
            let opt =
              List.find_opt
                (fun lim ->
                  match string_member lim "type" with
                  | Some t -> t = type_str
                  | None -> false)
                limits
            in
            match opt with
            | None -> None
            | Some lim ->
                let pct =
                  match float_member lim "percentage" with
                  | Some p -> p
                  | None -> (
                      match float_member lim "percent" with
                      | Some p -> p
                      | None -> 0.0)
                in
                let resets_at = float_member lim "reset_at" in
                Some { used_pct = pct; resets_at; window_duration_s = None }
          in
          let session = find_limit "TOKENS_LIMIT" in
          let monthly = find_limit "TIME_LIMIT" in
          let pq =
            {
              provider_name;
              state = Known { session; weekly = None; monthly };
              fetched_at = Unix.gettimeofday ();
            }
          in
          store_result pq;
          Lwt.return pq)
      (fun exn ->
        make_unknown provider_name
          (Printf.sprintf "fetch_failed:%s" (Printexc.to_string exn)))

(* Kimi quota via coding usage API. *)
let fetch_kimi ~api_key () =
  let open Lwt.Syntax in
  let provider_name = "kimi" in
  if api_key = "" then make_unknown provider_name "not_configured"
  else
    let uri = "https://api.kimi.com/coding/v1/usages" in
    let headers = [ ("Authorization", "Bearer " ^ api_key) ] in
    Lwt.catch
      (fun () ->
        let* status, body = Http_client.get ~uri ~headers in
        if status < 200 || status >= 300 then
          make_unknown provider_name
            (Printf.sprintf "fetch_failed:HTTP %d" status)
        else
          let j = Yojson.Safe.from_string body in
          let window_from_limits window_str dur_s =
            try
              let limits = Yojson.Safe.Util.(j |> member "limits" |> to_list) in
              let found =
                List.find_opt
                  (fun lim ->
                    match string_member lim "window" with
                    | Some w -> w = window_str
                    | None -> false)
                  limits
              in
              match found with
              | None -> None
              | Some lim ->
                  let detail = Yojson.Safe.Util.(lim |> member "detail") in
                  let used =
                    Yojson.Safe.Util.(detail |> member "used" |> to_float)
                  in
                  let limit =
                    Yojson.Safe.Util.(detail |> member "limit" |> to_float)
                  in
                  let resets_at = float_member lim "reset_at" in
                  if limit > 0.0 then
                    Some
                      {
                        used_pct = used /. limit *. 100.0;
                        resets_at;
                        window_duration_s = Some dur_s;
                      }
                  else None
            with _ -> None
          in
          let session =
            match window_from_limits "5h" 18000.0 with
            | Some _ as s -> s
            | None -> (
                try
                  let used =
                    Yojson.Safe.Util.(
                      j |> member "usage" |> member "used" |> to_float)
                  in
                  let limit =
                    Yojson.Safe.Util.(
                      j |> member "usage" |> member "limit" |> to_float)
                  in
                  if limit > 0.0 then
                    Some
                      {
                        used_pct = used /. limit *. 100.0;
                        resets_at = None;
                        window_duration_s = Some 18000.0;
                      }
                  else None
                with _ -> None)
          in
          let weekly = window_from_limits "7d" 604800.0 in
          let pq =
            {
              provider_name;
              state = Known { session; weekly; monthly = None };
              fetched_at = Unix.gettimeofday ();
            }
          in
          store_result pq;
          Lwt.return pq)
      (fun exn ->
        make_unknown provider_name
          (Printf.sprintf "fetch_failed:%s" (Printexc.to_string exn)))

(* Cursor: extract JWT from state.vscdb, build session token, call usage API.
   vscdb path: ~/.config/Cursor/User/globalStorage/state.vscdb
   SQL: SELECT value FROM ItemTable WHERE key = 'cursorAuth/accessToken'
   Session token: {userId}%3A%3A{jwtToken}  (userId = sub.split('|')[1])
   API: GET https://cursor.com/api/usage?user={userId}
   Auth: Cookie WorkosCursorSessionToken={sessionToken} *)

let parse_jwt_payload_claims token =
  match String.split_on_char '.' token with
  | [ _header; payload; _sig ] -> (
      let rem = String.length payload mod 4 in
      let padding = if rem = 0 then "" else String.make (4 - rem) '=' in
      let translated = Bytes.of_string (payload ^ padding) in
      Bytes.iteri
        (fun i -> function
          | '-' -> Bytes.set translated i '+'
          | '_' -> Bytes.set translated i '/'
          | _ -> ())
        translated;
      match Base64.decode (Bytes.to_string translated) with
      | Ok decoded -> (
          try Some (Yojson.Safe.from_string decoded) with _ -> None)
      | Error _ -> None)
  | _ -> None

let cursor_vscdb_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat home ".config/Cursor/User/globalStorage/state.vscdb"

let read_cursor_jwt () =
  let path = cursor_vscdb_path () in
  if not (Sys.file_exists path) then None
  else
    try
      let db = Sqlite3.db_open ~mode:`READONLY path in
      let token_ref = ref None in
      let _rc =
        Sqlite3.exec db
          "SELECT value FROM ItemTable WHERE key = 'cursorAuth/accessToken'"
          ~cb:(fun row _ ->
            match row with [| Some v |] -> token_ref := Some v | _ -> ())
      in
      (try Sqlite3.db_close db |> ignore with _ -> ());
      !token_ref
    with _ -> None

let build_cursor_session_token jwt_token =
  match parse_jwt_payload_claims jwt_token with
  | None -> None
  | Some claims -> (
      match string_member claims "sub" with
      | None -> None
      | Some sub ->
          let user_id =
            match String.split_on_char '|' sub with
            | _ :: id :: _ -> id
            | _ -> sub
          in
          Some (user_id ^ "%3A%3A" ^ jwt_token))

let fetch_cursor () =
  let open Lwt.Syntax in
  let provider_name = "cursor" in
  match read_cursor_jwt () with
  | None -> make_unknown provider_name "no_api"
  | Some jwt_token -> (
      match build_cursor_session_token jwt_token with
      | None -> make_unknown provider_name "no_api"
      | Some session_token ->
          (* user_id is the part before %3A%3A *)
          let user_id = List.hd (String.split_on_char '%' session_token) in
          let uri =
            Printf.sprintf "https://cursor.com/api/usage?user=%s" user_id
          in
          let headers =
            [
              ("Cookie", "WorkosCursorSessionToken=" ^ session_token);
              ("Content-Type", "application/json");
              ( "User-Agent",
                "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36" );
              ("Accept", "*/*");
            ]
          in
          Lwt.catch
            (fun () ->
              let* status, body = Http_client.get ~uri ~headers in
              if status < 200 || status >= 300 then
                make_unknown provider_name
                  (Printf.sprintf "fetch_failed:HTTP %d" status)
              else
                let j = Yojson.Safe.from_string body in
                let gpt4 =
                  try Yojson.Safe.Util.(j |> member "gpt-4") with _ -> `Null
                in
                let num_req =
                  try Yojson.Safe.Util.(gpt4 |> member "numRequests" |> to_int)
                  with _ -> 0
                in
                let max_req =
                  try
                    Yojson.Safe.Util.(
                      gpt4 |> member "maxRequestUsage" |> to_int)
                  with _ -> 0
                in
                if max_req <= 0 then make_unknown provider_name "no_api"
                else
                  let used_pct =
                    float_of_int num_req /. float_of_int max_req *. 100.0
                  in
                  let start_ts =
                    match string_member j "startOfMonth" with
                    | Some s -> parse_iso8601 s
                    | None -> None
                  in
                  let resets_at =
                    Option.map (fun ts -> ts +. 2592000.0) start_ts
                  in
                  let monthly =
                    Some
                      {
                        used_pct;
                        resets_at;
                        window_duration_s = Some 2592000.0;
                      }
                  in
                  let pq =
                    {
                      provider_name;
                      state = Known { session = None; weekly = None; monthly };
                      fetched_at = Unix.gettimeofday ();
                    }
                  in
                  store_result pq;
                  Lwt.return pq)
            (fun exn ->
              make_unknown provider_name
                (Printf.sprintf "fetch_failed:%s" (Printexc.to_string exn))))

(* ── Provider detection helpers ──────────────────────────────────────────── *)

let string_has_substring s sub =
  let ls = String.length s and lsub = String.length sub in
  if lsub = 0 then true
  else if ls < lsub then false
  else
    let rec go i =
      if i > ls - lsub then false
      else if String.sub s i lsub = sub then true
      else go (i + 1)
    in
    go 0

(* Fetch quota for a single provider given its clawq config entry. *)
let fetch_for_provider ~(config : Runtime_config.provider_config) ~name () =
  let open Lwt.Syntax in
  if not config.quota_check_enabled then make_unknown name "not_configured"
  else
    let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
    let lname = String.lowercase_ascii name in
    let lkind =
      Option.value ~default:"" config.kind |> String.lowercase_ascii
    in
    let lurl =
      Option.value ~default:"" config.base_url |> String.lowercase_ascii
    in
    let auto_cred_path dir file =
      match config.quota_credentials_file with
      | Some f -> f
      | None -> Filename.concat (Filename.concat home dir) file
    in
    let is_anthropic =
      lkind = "anthropic" || lname = "anthropic"
      || String.length config.api_key >= 7
         && String.sub config.api_key 0 7 = "sk-ant-"
    in
    let is_codex =
      lkind = "openai-codex" || lkind = "codex" || lname = "codex"
      || lname = "openai-codex"
    in
    let is_zai =
      lname = "zai" || lname = "zai_coding" || string_has_substring lurl "z.ai"
    in
    let is_kimi = lname = "kimi" || string_has_substring lurl "kimi" in
    let is_cursor = lname = "cursor" in
    if is_anthropic then (
      let* pq =
        fetch_anthropic
          ~credentials_file:(auto_cred_path ".claude" ".credentials.json")
          ()
      in
      (* Store under the actual provider name the user configured *)
      let pq' = { pq with provider_name = name } in
      store_result pq';
      Lwt.return pq')
    else if is_codex then (
      let* pq =
        fetch_codex ~credentials_file:(auto_cred_path ".codex" "auth.json") ()
      in
      let pq' = { pq with provider_name = name } in
      store_result pq';
      Lwt.return pq')
    else if is_zai then fetch_zai ~api_key:config.api_key ()
    else if is_kimi then
      let api_key = config.api_key in
      fetch_kimi ~api_key ()
    else if is_cursor then fetch_cursor ()
    else make_unknown name "no_api"

(* Refresh all configured providers and return updated quota list. *)
let refresh_all ~(config : Runtime_config.t) () =
  let open Lwt.Syntax in
  set_cache_ttl config.quota_cache_ttl_s;
  let* results =
    Lwt_list.map_p
      (fun (name, pc) -> fetch_for_provider ~config:pc ~name ())
      config.providers
  in
  Lwt.return results
