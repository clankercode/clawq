(* Room-scoped MCP access and credential isolation. *)

type transport = Http | Stdio

type grant = {
  server : string;
  transport : transport;
  credential_handle : string option;
}

type room_scope = {
  room_id : string;
  allowed_servers : grant list;
  access_revision : string;
}

type lease = {
  room_id : string;
  server : string;
  credential_handle : string;
  lease_id : string;
  access_revision : string;
}

let make_scope ~room_id ~allowed_servers ~access_revision =
  { room_id; allowed_servers; access_revision }

let find_grant (scope : room_scope) server =
  List.find_opt (fun (g : grant) -> g.server = server) scope.allowed_servers

let filter_servers ~scope ~server_names =
  List.filter (fun name -> Option.is_some (find_grant scope name)) server_names

let may_invoke ~scope ~server =
  match find_grant scope server with
  | Some g -> Ok g
  | None ->
      Error
        (Printf.sprintf
           "MCP server '%s' is not granted to room '%s' under access revision \
            %s"
           server scope.room_id scope.access_revision)

let lease_http_credential ~scope ~server =
  match may_invoke ~scope ~server with
  | Error e -> Error e
  | Ok g -> (
      match g.transport with
      | Stdio -> Error "HTTP credential lease not applicable to stdio transport"
      | Http -> (
          match g.credential_handle with
          | None ->
              Error
                (Printf.sprintf
                   "MCP server '%s' has no credential handle for HTTP lease"
                   server)
          | Some handle ->
              let lease_id =
                Printf.sprintf "mclease_%s_%s_%d" scope.room_id server
                  (Random.int 1_000_000)
              in
              Ok
                {
                  room_id = scope.room_id;
                  server;
                  credential_handle = handle;
                  lease_id;
                  access_revision = scope.access_revision;
                }))

let stdio_client_key ~scope ~server =
  match may_invoke ~scope ~server with
  | Error e -> Error e
  | Ok g -> (
      match g.transport with
      | Http -> Error "stdio client key not applicable to HTTP transport"
      | Stdio -> (
          match g.credential_handle with
          | None ->
              (* Non-credential stdio can be scope-keyed by room alone. *)
              Ok (Printf.sprintf "stdio:%s:%s" scope.room_id server)
          | Some handle ->
              (* Credential-bearing stdio must include the handle in the key so
                 rooms never share a credential-bearing client process. *)
              Ok (Printf.sprintf "stdio:%s:%s:%s" scope.room_id server handle)))

let scopes_isolated ~a ~b ~server =
  let in_a = Option.is_some (find_grant a server) in
  let in_b = Option.is_some (find_grant b server) in
  (* Isolation for a server: at most one of the two rooms may hold it, or if
     both hold it they must use distinct credential handles. *)
  match (in_a, in_b) with
  | false, false -> true
  | true, false | false, true -> true
  | true, true -> (
      match (find_grant a server, find_grant b server) with
      | Some ga, Some gb -> ga.credential_handle <> gb.credential_handle
      | _ -> true)
