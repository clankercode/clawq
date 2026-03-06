(* config_validate.ml — Optional credential validation via HTTP *)

let test_provider ~name:_ ~api_key ~base_url =
  if api_key = "" then
    Lwt.return_ok "Skipped remote validation (provider uses non-API-key auth)"
  else begin
    let uri = base_url ^ "/models" in
    let headers = [ ("Authorization", "Bearer " ^ api_key) ] in
    Lwt.catch
      (fun () ->
        let open Lwt.Syntax in
        let* result = Http_client.get ~uri ~headers in
        match result with
        | code, body_s when code >= 200 && code < 300 ->
            Lwt.return_ok
              (Printf.sprintf "OK (HTTP %d, %d bytes)" code
                 (String.length body_s))
        | code, body_s ->
            Lwt.return_error
              (Printf.sprintf "HTTP %d: %s" code
                 (String.sub body_s 0 (min 200 (String.length body_s)))))
      (fun exn -> Lwt.return_error (Printexc.to_string exn))
  end
