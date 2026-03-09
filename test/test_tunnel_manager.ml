(* Tests for Tunnel_manager *)

let default_tunnel = Runtime_config.default.tunnel

let test_config_equal_same () =
  Alcotest.(check bool)
    "identical configs" true
    (Tunnel_manager.tunnel_config_equal default_tunnel default_tunnel)

let test_config_equal_different_provider () =
  let b = { default_tunnel with provider = "ngrok" } in
  Alcotest.(check bool)
    "different provider" false
    (Tunnel_manager.tunnel_config_equal default_tunnel b)

let test_config_equal_different_enabled () =
  let b = { default_tunnel with enabled = true } in
  Alcotest.(check bool)
    "different enabled" false
    (Tunnel_manager.tunnel_config_equal default_tunnel b)

let test_config_equal_different_url () =
  let b = { default_tunnel with url = "https://example.com" } in
  Alcotest.(check bool)
    "different url" false
    (Tunnel_manager.tunnel_config_equal default_tunnel b)

let test_config_equal_different_managed () =
  let b = { default_tunnel with managed = true } in
  Alcotest.(check bool)
    "different managed" false
    (Tunnel_manager.tunnel_config_equal default_tunnel b)

let test_config_equal_different_tunnel_name () =
  let a = { default_tunnel with tunnel_name = "a" } in
  let b = { default_tunnel with tunnel_name = "b" } in
  Alcotest.(check bool)
    "different tunnel_name" false
    (Tunnel_manager.tunnel_config_equal a b)

let test_create_idle () =
  let mgr = Tunnel_manager.create () in
  Alcotest.(check (option string)) "no url" None (Tunnel_manager.get_url mgr);
  Alcotest.(check (option int)) "no pid" None (Tunnel_manager.get_pid mgr)

let test_apply_disabled_stays_idle () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let mgr = Tunnel_manager.create () in
     let called = ref false in
     let* () =
       Tunnel_manager.apply_config mgr
         ~config:{ default_tunnel with enabled = false } ~port:8080
         ~on_url:(fun _ -> called := true)
     in
     Alcotest.(check bool) "on_url not called" false !called;
     Alcotest.(check (option string))
       "still no url" None
       (Tunnel_manager.get_url mgr);
     Lwt.return_unit)

let test_apply_static_url () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let mgr = Tunnel_manager.create () in
     let received_url = ref None in
     let cfg =
       {
         default_tunnel with
         enabled = true;
         provider = "cloudflare";
         url = "https://static.example.com";
         managed = false;
       }
     in
     let* () =
       Tunnel_manager.apply_config mgr ~config:cfg ~port:8080 ~on_url:(fun u ->
           received_url := u)
     in
     (* The manager should have started - url gets set via on_url callback *)
     Alcotest.(check (option string))
       "received url" (Some "https://static.example.com") !received_url;
     let* () = Tunnel_manager.stop mgr in
     Lwt.return_unit)

let test_stop_from_idle () =
  Lwt_main.run
    (let mgr = Tunnel_manager.create () in
     Tunnel_manager.stop mgr)

let test_status_json_idle () =
  let mgr = Tunnel_manager.create () in
  let json = Tunnel_manager.status_json mgr in
  let open Yojson.Safe.Util in
  let state = json |> member "state" |> to_string in
  Alcotest.(check string) "state is idle" "idle" state;
  let provider = json |> member "provider" in
  Alcotest.(check bool) "provider is null" true (provider = `Null)

let test_status_json_active () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let mgr = Tunnel_manager.create () in
     let cfg =
       {
         default_tunnel with
         enabled = true;
         provider = "cloudflare";
         url = "https://test.example.com";
         managed = false;
       }
     in
     let* () =
       Tunnel_manager.apply_config mgr ~config:cfg ~port:8080 ~on_url:(fun _ ->
           ())
     in
     let json = Tunnel_manager.status_json mgr in
     let open Yojson.Safe.Util in
     let state = json |> member "state" |> to_string in
     Alcotest.(check string) "state is active" "active" state;
     let provider = json |> member "provider" |> to_string in
     Alcotest.(check string) "provider" "cloudflare" provider;
     let* () = Tunnel_manager.stop mgr in
     Lwt.return_unit)

let test_idempotent_apply () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let mgr = Tunnel_manager.create () in
     let call_count = ref 0 in
     let cfg =
       {
         default_tunnel with
         enabled = true;
         provider = "cloudflare";
         url = "https://idem.example.com";
         managed = false;
       }
     in
     let on_url _ = incr call_count in
     let* () = Tunnel_manager.apply_config mgr ~config:cfg ~port:8080 ~on_url in
     let first_count = !call_count in
     (* Second apply with same config should be no-op *)
     let* () = Tunnel_manager.apply_config mgr ~config:cfg ~port:8080 ~on_url in
     Alcotest.(check int) "no extra on_url call" first_count !call_count;
     let* () = Tunnel_manager.stop mgr in
     Lwt.return_unit)

let test_restart_unconditional () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let mgr = Tunnel_manager.create () in
     let call_count = ref 0 in
     let cfg =
       {
         default_tunnel with
         enabled = true;
         provider = "cloudflare";
         url = "https://restart.example.com";
         managed = false;
       }
     in
     let on_url _ = incr call_count in
     (* First apply starts the tunnel *)
     let* () = Tunnel_manager.apply_config mgr ~config:cfg ~port:8080 ~on_url in
     let count_after_apply = !call_count in
     (* Second apply with same config should be no-op *)
     let* () = Tunnel_manager.apply_config mgr ~config:cfg ~port:8080 ~on_url in
     Alcotest.(check int) "apply is idempotent" count_after_apply !call_count;
     (* restart with same config should still stop and restart *)
     let* () = Tunnel_manager.restart mgr ~config:cfg ~port:8080 ~on_url in
     Alcotest.(check bool)
       "restart triggered new on_url calls" true
       (!call_count > count_after_apply);
     let* () = Tunnel_manager.stop mgr in
     Lwt.return_unit)

let test_config_change_detected () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let mgr = Tunnel_manager.create () in
     let urls = ref [] in
     let cfg_a =
       {
         default_tunnel with
         enabled = true;
         provider = "cloudflare";
         url = "https://a.example.com";
         managed = false;
       }
     in
     let cfg_b =
       {
         default_tunnel with
         enabled = true;
         provider = "cloudflare";
         url = "https://b.example.com";
         managed = false;
       }
     in
     let on_url u = urls := u :: !urls in
     let* () =
       Tunnel_manager.apply_config mgr ~config:cfg_a ~port:8080 ~on_url
     in
     let* () =
       Tunnel_manager.apply_config mgr ~config:cfg_b ~port:8080 ~on_url
     in
     (* Should have received multiple on_url calls indicating change *)
     Alcotest.(check bool)
       "multiple url notifications" true
       (List.length !urls >= 2);
     let* () = Tunnel_manager.stop mgr in
     Lwt.return_unit)

let suite =
  [
    Alcotest.test_case "config_equal same" `Quick test_config_equal_same;
    Alcotest.test_case "config_equal different provider" `Quick
      test_config_equal_different_provider;
    Alcotest.test_case "config_equal different enabled" `Quick
      test_config_equal_different_enabled;
    Alcotest.test_case "config_equal different url" `Quick
      test_config_equal_different_url;
    Alcotest.test_case "config_equal different managed" `Quick
      test_config_equal_different_managed;
    Alcotest.test_case "config_equal different tunnel_name" `Quick
      test_config_equal_different_tunnel_name;
    Alcotest.test_case "create is idle" `Quick test_create_idle;
    Alcotest.test_case "apply disabled stays idle" `Quick
      test_apply_disabled_stays_idle;
    Alcotest.test_case "apply static url" `Quick test_apply_static_url;
    Alcotest.test_case "stop from idle" `Quick test_stop_from_idle;
    Alcotest.test_case "status json idle" `Quick test_status_json_idle;
    Alcotest.test_case "status json active" `Quick test_status_json_active;
    Alcotest.test_case "idempotent apply" `Quick test_idempotent_apply;
    Alcotest.test_case "restart unconditional" `Quick test_restart_unconditional;
    Alcotest.test_case "config change detected" `Quick
      test_config_change_detected;
  ]
