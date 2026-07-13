(* Lwt HTTP ingress for the browser half of GitHub App manifest setup.
   The core exchange remains synchronous/atomic; this adapter performs the
   required remote conversion and authenticated installation snapshot first,
   then supplies fixed verified results to that core. *)

open Lwt.Syntax

type client = {
  post :
    url:string ->
    headers:(string * string) list ->
    body:string ->
    (int * string, string) result Lwt.t;
  get :
    url:string ->
    headers:(string * string) list ->
    (int * string, string) result Lwt.t;
}

let request ~meth ~url ~headers ?body () =
  let body = Option.value body ~default:"" in
  Lwt.catch
    (fun () ->
      Lwt_unix.with_timeout 180.0 (fun () ->
          let uri = Uri.of_string url in
          let headers = Cohttp.Header.of_list headers in
          let call =
            match meth with
            | `GET -> Cohttp_lwt_unix.Client.get ~headers uri
            | `POST ->
                Cohttp_lwt_unix.Client.post ~headers
                  ~body:(Cohttp_lwt.Body.of_string body)
                  uri
          in
          let* response, response_body = call in
          let status =
            Cohttp.Response.status response |> Cohttp.Code.code_of_status
          in
          let* response_body = Cohttp_lwt.Body.to_string response_body in
          Lwt.return (Ok (status, response_body))))
    (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

let live_client =
  {
    post =
      (fun ~url ~headers ~body -> request ~meth:`POST ~url ~headers ~body ());
    get = (fun ~url ~headers -> request ~meth:`GET ~url ~headers ());
  }

let github_headers ~authorization =
  [
    ("Authorization", authorization);
    ("Accept", "application/vnd.github+json");
    ("User-Agent", "clawq-github-app-setup");
    ("X-GitHub-Api-Version", "2022-11-28");
  ]

let error_for_status ~operation status =
  Printf.sprintf "GitHub %s returned HTTP %d" operation status

let json_int j field =
  match Yojson.Safe.Util.member field j with
  | `Int value -> Ok value
  | `Intlit value -> (
      try Ok (int_of_string value)
      with Failure _ -> Error (Printf.sprintf "%s is not an integer" field))
  | _ -> Error (Printf.sprintf "%s is missing" field)

let json_string j field =
  match Yojson.Safe.Util.member field j with
  | `String value when String.trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "%s is missing" field)

let parse_conversion_identity body =
  try
    let json = Yojson.Safe.from_string body in
    match (json_int json "id", json_string json "pem") with
    | Ok app_id, Ok private_key_pem -> Ok (app_id, private_key_pem)
    | Error error, _ | _, Error error ->
        Error ("invalid conversion response: " ^ error)
  with Yojson.Json_error _ -> Error "invalid conversion response JSON"

let app_jwt ~app_id ~private_key_pem =
  match Github_app_token.parse_rsa_priv private_key_pem with
  | Error error -> Error ("converted App private key is invalid: " ^ error)
  | Ok key -> Ok (Github_app_token.generate_jwt ~key ~app_id)

let parse_installation ~app_id ~installation_id ~repositories body =
  try
    let json = Yojson.Safe.from_string body in
    match
      ( json_int json "id",
        json_int json "app_id",
        Github_app_installation_scope.account_of_json
          (Yojson.Safe.Util.member "account" json),
        Github_app_installation_scope.permissions_of_json
          (Yojson.Safe.Util.member "permissions" json) )
    with
    | ( Ok returned_installation_id,
        Ok returned_app_id,
        Ok account,
        Ok permissions ) -> (
        if returned_installation_id <> installation_id then
          Error "authenticated installation response id does not match callback"
        else if returned_app_id <> app_id then
          Error
            "authenticated installation response App id does not match \
             conversion"
        else
          let selection =
            match Yojson.Safe.Util.member "repository_selection" json with
            | `String "all" -> Ok Github_app_installation_scope.All_repos
            | `String "selected" ->
                Ok Github_app_installation_scope.Selected_repos
            | _ ->
                Error "authenticated installation omitted repository_selection"
          in
          match selection with
          | Error _ as error -> error
          | Ok selection ->
              let status =
                match Yojson.Safe.Util.member "suspended_at" json with
                | `Null -> Github_app_installation_scope.Active
                | _ -> Github_app_installation_scope.Suspended { reason = None }
              in
              Ok
                (Github_app_installation_scope.with_revision
                   {
                     installation_id;
                     app_id = Some app_id;
                     account;
                     selection;
                     repositories;
                     revoked_repositories = [];
                     permissions;
                     status;
                     revision = "";
                     updated_at = Time_util.iso8601_utc ();
                   }))
    | Error error, _, _, _
    | _, Error error, _, _
    | _, _, Error error, _
    | _, _, _, Error error ->
        Error ("invalid authenticated installation: " ^ error)
  with Yojson.Json_error _ -> Error "invalid authenticated installation JSON"

let fetch_installation_token ~client ~jwt ~installation_id =
  let url =
    Printf.sprintf "%s/app/installations/%d/access_tokens"
      (Github_app_token.github_api_base ())
      installation_id
  in
  let* response =
    client.post ~url
      ~headers:(github_headers ~authorization:("Bearer " ^ jwt))
      ~body:"{}"
  in
  match response with
  | Error error ->
      Lwt.return (Error ("installation token request failed: " ^ error))
  | Ok (status, _) when status < 200 || status >= 300 ->
      Lwt.return
        (Error (error_for_status ~operation:"installation token request" status))
  | Ok (_, body) -> (
      try
        let json = Yojson.Safe.from_string body in
        Lwt.return
          (match json_string json "token" with
          | Ok token -> Ok token
          | Error error ->
              Error ("invalid installation token response: " ^ error))
      with Yojson.Json_error _ ->
        Lwt.return (Error "invalid installation token response JSON"))

let fetch_selected_repositories ~client ~jwt ~installation_id =
  let* token = fetch_installation_token ~client ~jwt ~installation_id in
  match token with
  | Error _ as error -> Lwt.return error
  | Ok token -> (
      let url =
        Printf.sprintf "%s/installation/repositories?per_page=100"
          (Github_app_token.github_api_base ())
      in
      let* response =
        client.get ~url
          ~headers:(github_headers ~authorization:("token " ^ token))
      in
      match response with
      | Error error ->
          Lwt.return
            (Error ("installation repositories request failed: " ^ error))
      | Ok (status, _) when status < 200 || status >= 300 ->
          Lwt.return
            (Error
               (error_for_status ~operation:"installation repositories request"
                  status))
      | Ok (_, body) -> (
          try
            let json = Yojson.Safe.from_string body in
            let repositories =
              Github_app_installation_scope.repos_of_json
                (Yojson.Safe.Util.member "repositories" json)
            in
            match repositories with
            | Error error ->
                Lwt.return
                  (Error ("invalid installation repositories response: " ^ error))
            | Ok repositories -> (
                let total_count =
                  match Yojson.Safe.Util.member "total_count" json with
                  | `Int count -> Some count
                  | `Intlit count -> int_of_string_opt count
                  | _ -> None
                in
                match total_count with
                | Some count when count > List.length repositories ->
                    Lwt.return
                      (Error
                         "installation repository verification requires \
                          pagination; refusing an incomplete \
                          selected-repository scope")
                | _ -> Lwt.return (Ok repositories))
          with Yojson.Json_error _ ->
            Lwt.return (Error "invalid installation repositories response JSON")
          ))

let verify_installation ~client ~app_id ~private_key_pem ~installation_id =
  match app_jwt ~app_id ~private_key_pem with
  | Error error -> Lwt.return (Error error)
  | Ok jwt -> (
      let url =
        Printf.sprintf "%s/app/installations/%d"
          (Github_app_token.github_api_base ())
          installation_id
      in
      let* response =
        client.get ~url
          ~headers:(github_headers ~authorization:("Bearer " ^ jwt))
      in
      match response with
      | Error error ->
          Lwt.return
            (Error ("installation verification request failed: " ^ error))
      | Ok (status, _) when status < 200 || status >= 300 ->
          Lwt.return
            (Error
               (error_for_status ~operation:"installation verification" status))
      | Ok (_, body) -> (
          let selection =
            try
              let json = Yojson.Safe.from_string body in
              match Yojson.Safe.Util.member "repository_selection" json with
              | `String "all" -> Ok Github_app_installation_scope.All_repos
              | `String "selected" ->
                  Ok Github_app_installation_scope.Selected_repos
              | _ ->
                  Error
                    "authenticated installation omitted repository_selection"
            with Yojson.Json_error _ ->
              Error "invalid authenticated installation JSON"
          in
          match selection with
          | Error _ as error -> Lwt.return error
          | Ok Github_app_installation_scope.All_repos ->
              Lwt.return
                (parse_installation ~app_id ~installation_id ~repositories:[]
                   body)
          | Ok Github_app_installation_scope.Selected_repos -> (
              let* repositories =
                fetch_selected_repositories ~client ~jwt ~installation_id
              in
              match repositories with
              | Error _ as error -> Lwt.return error
              | Ok repositories ->
                  Lwt.return
                    (parse_installation ~app_id ~installation_id ~repositories
                       body))))

let exchange ~db ?(client = live_client) ?store_secret
    ?(now = Unix.gettimeofday ()) ~code ~state ~installation_id ?setup_action ()
    =
  match Github_app_setup_tx.find_by_state ~db ~state with
  | Error error -> Lwt.return (Error error)
  | Ok None -> Lwt.return (Error "unknown setup state: no matching transaction")
  | Ok (Some tx) -> (
      let conversion_url = Github_app_setup_callback.conversion_url ~code in
      let* conversion =
        client.post ~url:conversion_url
          ~headers:
            [
              ("Accept", "application/vnd.github+json");
              ("User-Agent", "clawq-github-app-setup");
              ("X-GitHub-Api-Version", "2022-11-28");
            ]
          ~body:""
      in
      match conversion with
      | Error error ->
          Lwt.return (Error ("GitHub conversion request failed: " ^ error))
      | Ok (status, _) when status < 200 || status >= 300 ->
          Lwt.return (Error (error_for_status ~operation:"conversion" status))
      | Ok (_, conversion_body) -> (
          match parse_conversion_identity conversion_body with
          | Error _ as error -> Lwt.return error
          | Ok (app_id, private_key_pem) -> (
              let* verified_installation =
                verify_installation ~client ~app_id ~private_key_pem
                  ~installation_id
              in
              match verified_installation with
              | Error _ as error -> Lwt.return error
              | Ok verified_installation ->
                  let http_post ~url ~headers:_ ~body:_ =
                    if String.equal url conversion_url then
                      Ok (201, conversion_body)
                    else Error "unexpected conversion URL"
                  in
                  let verify_installation ~app_id:verified_app_id
                      ~private_key_pem:verified_pem
                      ~installation_id:verified_installation_id =
                    if
                      verified_app_id <> app_id
                      || verified_pem <> private_key_pem
                      || verified_installation_id <> installation_id
                    then
                      Error "callback verifier arguments changed unexpectedly"
                    else Ok verified_installation
                  in
                  Lwt.return
                    (Github_app_setup_callback.exchange ~db ~http_post
                       ?store_secret ~now ~verify_installation
                       {
                         Github_app_setup_callback.code;
                         state;
                         callback_path =
                           Some Github_app_setup_tx.default_callback_path;
                         expected_bind = Some tx.bind;
                         expected_principal_id = Some tx.principal.id;
                         installation_id = Some installation_id;
                         setup_action;
                       }))))
