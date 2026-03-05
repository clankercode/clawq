# GitHub Auth Abstraction

## Design goal

PAT works today. GitHub App must be addable later with **zero changes** to `github_webhook.ml`, `github.ml`, or `http_server.ml`. Changes confined to: `runtime_config.ml`, `config_loader.ml`, `github_api.ml`.

## Type design

```ocaml
(* In runtime_config.ml *)

type github_auth =
  | GithubPat of string
  (* Future, not in v1: *)
  (* | GithubApp of github_app_credentials *)

(* Future type, not in v1: *)
type github_app_credentials = {
  app_id : int;
  private_key_path : string;  (* path to .pem file *)
  installation_id : int;       (* per-repo installation *)
}
```

## Auth header resolution (in `github_api.ml`)

```ocaml
(* v1 — PAT only *)
let auth_headers (auth : Runtime_config.github_auth) =
  match auth with
  | GithubPat token ->
      let redacted = String.sub token 0 (min 8 (String.length token)) ^ "..." in
      Logs.debug (fun m -> m "GitHub auth: PAT %s" redacted);
      Lwt.return [ ("Authorization", "Bearer " ^ token) ]
  (* Future GitHub App case: *)
  (* | GithubApp creds ->
       let* token = get_installation_token creds in
       Lwt.return [ ("Authorization", "Bearer " ^ token) ] *)
```

The returned `(string * string) list` is passed directly to `Http_client.post_json_with_headers`. All API functions accept `auth:Runtime_config.github_auth` and call `auth_headers` internally.

## JSON config parsing (in `config_loader.ml`)

```json
{
  "auth": {
    "type": "pat",
    "token": "ghp_..."
  }
}
```

Future GitHub App:
```json
{
  "auth": {
    "type": "github_app",
    "app_id": 12345,
    "private_key_path": "~/.clawq/github-app.pem",
    "installation_id": 67890
  }
}
```

Parsing:
```ocaml
let parse_github_auth json =
  let open Yojson.Safe.Util in
  let typ = json |> member "type" |> to_string in
  match typ with
  | "pat" ->
      let token = json |> member "token" |> to_string in
      Runtime_config.GithubPat token
  | "github_app" ->
      failwith "github_app auth not yet supported"
      (* Future: parse app_id, private_key_path, installation_id *)
  | other ->
      failwith ("Unknown github auth type: " ^ other)
```

## Adding GitHub App later (checklist)

When the GitHub App task lands:

1. `runtime_config.ml`: uncomment `GithubApp` variant and `github_app_credentials` type
2. `config_loader.ml`: implement `"github_app"` branch in `parse_github_auth`
3. `github_api.ml`: implement `get_installation_token` (JWT → POST /app/installations/{id}/access_tokens → bearer token with expiry caching)
4. Uncomment `GithubApp` match arm in `auth_headers`
5. Add tests for installation token fetch and caching

No other files change. The auth abstraction is complete.

## Why not GitHub App in v1?

GitHub App requires:
- Registering an app in GitHub org settings
- Generating a private key
- Computing JWTs signed with RS256 (would need `mirage-crypto-pk` or `jose` opam dep)
- Token caching (installation tokens expire after 1 hour)
- Per-installation_id config (different repos may be different installations)

This is meaningful added complexity and a new opam dep (RS256 JWT). PAT is sufficient for single-user/team use. The abstraction ensures no rework when App support is added.
