let restore_env previous =
  match previous with
  | Some v -> Unix.putenv "OCAMLRUNPARAM" v
  | None -> Unix.putenv "OCAMLRUNPARAM" ""

(* [Unix.putenv] cannot remove a variable, so an absent original is simulated
   with an empty string; Gc_init.env_sets_space_overhead treats that as unset. *)
let with_ocamlrunparam value f =
  let previous = Sys.getenv_opt "OCAMLRUNPARAM" in
  Unix.putenv "OCAMLRUNPARAM" value;
  Fun.protect ~finally:(fun () -> restore_env previous) f

let test_apply_sets_space_overhead () =
  let previous = Gc.get () in
  Fun.protect
    ~finally:(fun () -> Gc.set previous)
    (fun () ->
      with_ocamlrunparam "" (fun () ->
          Gc_init.apply ();
          Alcotest.(check int)
            "space_overhead tuned to 60" 60 (Gc.get ()).Gc.space_overhead))

let test_env_override_wins () =
  let previous = Gc.get () in
  Fun.protect
    ~finally:(fun () -> Gc.set previous)
    (fun () ->
      (* The runtime applies OCAMLRUNPARAM before main; simulate that by
         seeding Gc with the env value before apply runs. *)
      with_ocamlrunparam "o=37" (fun () ->
          Gc.set { previous with Gc.space_overhead = 37 };
          Gc_init.apply ();
          Alcotest.(check int)
            "explicit env o= respected" 37 (Gc.get ()).Gc.space_overhead))

let test_logs_applied_settings () =
  let previous = Gc.get () in
  let logged = ref [] in
  Fun.protect
    ~finally:(fun () -> Gc.set previous)
    (fun () ->
      with_ocamlrunparam "" (fun () ->
          Gc_init.apply ~log_line:(fun msg -> logged := msg :: !logged) ();
          Alcotest.(check int) "exactly one log line" 1 (List.length !logged);
          Alcotest.(check bool)
            "log line reports space_overhead" true
            (match !logged with
            | [ msg ] -> Test_helpers.string_contains msg "space_overhead=60"
            | _ -> false)))

let test_unrelated_env_params_do_not_block_tuning () =
  let previous = Gc.get () in
  Fun.protect
    ~finally:(fun () -> Gc.set previous)
    (fun () ->
      (* Runtime params are case-sensitive; "O=" is a different parameter, so
         only an explicit lowercase "o=" counts as a space_overhead override. *)
      with_ocamlrunparam "v=0x400,i=32k,O=120" (fun () ->
          Gc_init.apply ();
          Alcotest.(check int)
            "unrelated params still tuned" 60 (Gc.get ()).Gc.space_overhead))

let suite =
  [
    Alcotest.test_case "apply sets space_overhead to 60" `Quick
      test_apply_sets_space_overhead;
    Alcotest.test_case "explicit OCAMLRUNPARAM o= wins" `Quick
      test_env_override_wins;
    Alcotest.test_case "unrelated OCAMLRUNPARAM params do not block tuning"
      `Quick test_unrelated_env_params_do_not_block_tuning;
    Alcotest.test_case "apply logs one line with applied settings" `Quick
      test_logs_applied_settings;
  ]
