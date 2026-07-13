(** Tests for visible App fallback and fail-closed user-required behavior
    (P21.M3.E2.T004). *)

module F = Github_attribution_fallback
module Policy = Github_attribution_policy

let expect_allow ?(mode = F.User) ?(used_app_fallback = false) decision =
  match decision with
  | F.Allow a ->
      Alcotest.(check string)
        "mode"
        (F.actor_mode_to_string mode)
        (F.actor_mode_to_string a.mode);
      Alcotest.(check bool)
        "used_app_fallback" used_app_fallback a.used_app_fallback;
      a
  | F.Deny d ->
      Alcotest.fail
        (Printf.sprintf "expected Allow, got Deny code=%s kind=%s" d.code
           (F.deny_kind_to_string d.kind))

let expect_deny ~code ?kind decision =
  match decision with
  | F.Deny d ->
      Alcotest.(check string) "code" code d.code;
      (match kind with
      | None -> ()
      | Some k ->
          Alcotest.(check string)
            "kind" (F.deny_kind_to_string k)
            (F.deny_kind_to_string d.kind));
      Alcotest.(check bool) "is_deny" true (F.is_deny decision);
      d
  | F.Allow a ->
      Alcotest.fail
        (Printf.sprintf "expected Deny, got Allow mode=%s"
           (F.actor_mode_to_string a.mode))

(* -------------------------------------------------------------------------- *)
(* User_preferred visible App fallback                                         *)
(* -------------------------------------------------------------------------- *)

let test_user_preferred_user_path () =
  let d =
    F.resolve
      (F.default_request ~action:"comment" ~preview_actor:F.Names_user
         ~user_path_available:true ())
  in
  let a = expect_allow ~mode:F.User ~used_app_fallback:false d in
  Alcotest.(check string)
    "attribution" "user_preferred"
    (Policy.attribution_to_string a.requirement.attribution)

let test_user_preferred_visible_app_fallback () =
  let d =
    F.resolve
      (F.default_request ~action:"comment" ~preview_actor:F.Names_app
         ~user_path_available:false ~app_path_available:true ())
  in
  ignore (expect_allow ~mode:F.App ~used_app_fallback:true d)

let test_user_preferred_app_fallback_requires_preview () =
  (* No user path and preview names user → stay on User (no silent App). *)
  let d =
    F.resolve
      (F.default_request ~action:"comment" ~preview_actor:F.Names_user
         ~user_path_available:false ~app_path_available:true ())
  in
  let a = expect_allow ~mode:F.User ~used_app_fallback:false d in
  Alcotest.(check bool)
    "policy permits fallback but not used without preview name" true
    (Policy.permits_app_fallback a.requirement.attribution);
  Alcotest.(check bool) "not app fallback" false a.used_app_fallback

let test_user_preferred_never_pat_fallback () =
  let d =
    F.resolve
      (F.default_request ~action:"label" ~preview_actor:F.Names_pat
         ~user_path_available:false ~app_path_available:true ())
  in
  ignore (expect_deny ~code:"pat_fallback_forbidden" ~kind:F.Reconfirmation d)

let test_user_preferred_app_path_must_be_available () =
  let d =
    F.resolve
      (F.default_request ~action:"comment" ~preview_actor:F.Names_app
         ~app_path_available:false ())
  in
  ignore (expect_deny ~code:"app_fallback_path_unavailable" ~kind:F.Repair d)

(* -------------------------------------------------------------------------- *)
(* User_required fail-closed                                                   *)
(* -------------------------------------------------------------------------- *)

let test_user_required_allows_user () =
  let d =
    F.resolve
      (F.default_request ~action:"merge" ~preview_actor:F.Names_user
         ~user_path_available:true ())
  in
  ignore (expect_allow ~mode:F.User ~used_app_fallback:false d)

let test_user_required_never_app_even_if_preview_names_app () =
  let d =
    F.resolve
      (F.default_request ~action:"merge" ~preview_actor:F.Names_app
         ~user_path_available:false ~app_path_available:true ())
  in
  ignore
    (expect_deny ~code:"user_required_no_fallback" ~kind:F.Reconfirmation d)

let test_user_required_never_pat () =
  let d =
    F.resolve
      (F.default_request ~action:"merge" ~preview_actor:F.Names_pat
         ~app_path_available:true ())
  in
  ignore (expect_deny ~code:"user_required_no_fallback" d)

let test_user_required_no_silent_app_when_user_missing () =
  (* Missing user path still resolves User (authorize reports binding repair);
     never App. *)
  let d =
    F.resolve
      (F.default_request ~action:"merge" ~preview_actor:F.Names_user
         ~user_path_available:false ~app_path_available:true ())
  in
  ignore (expect_allow ~mode:F.User ~used_app_fallback:false d)

(* -------------------------------------------------------------------------- *)
(* Attribution gate disabled                                                   *)
(* -------------------------------------------------------------------------- *)

let test_gate_disabled_blocks_user_required () =
  let d =
    F.resolve
      (F.default_request ~action:"merge" ~attribution_gate_enabled:false
         ~user_path_available:true ~app_path_available:true ())
  in
  ignore (expect_deny ~code:"attribution_gate_disabled" ~kind:F.Repair d)

let test_gate_disabled_blocks_user_preferred_no_app_fallback () =
  let d =
    F.resolve
      (F.default_request ~action:"comment" ~attribution_gate_enabled:false
         ~preview_actor:F.Names_app ~app_path_available:true ())
  in
  ignore (expect_deny ~code:"attribution_gate_disabled" ~kind:F.Repair d)

let test_gate_disabled_allows_pure_app_installation () =
  let req =
    Policy.
      {
        action = "ambient_read";
        tier = Low;
        attribution = App_installation;
        pilot_allowed = false;
      }
  in
  let d =
    F.resolve
      (F.default_request ~action:"ambient_read" ~requirement:req
         ~attribution_gate_enabled:false ~preview_actor:F.Names_app
         ~user_path_available:false ~app_path_available:true ())
  in
  ignore (expect_allow ~mode:F.App ~used_app_fallback:false d)

(* -------------------------------------------------------------------------- *)
(* Post-confirm authority loss                                                 *)
(* -------------------------------------------------------------------------- *)

let test_post_confirm_authority_loss_no_fallback () =
  let d =
    F.resolve
      (F.default_request ~action:"comment" ~preview_actor:F.Names_app
         ~post_confirm_authority_lost:true ~app_path_available:true
         ~phase:(F.Post_confirm { locked_mode = F.User })
         ())
  in
  ignore
    (expect_deny ~code:"post_confirm_authority_lost" ~kind:F.Reconfirmation d)

let test_post_confirm_authority_loss_user_required () =
  let d =
    F.resolve
      (F.default_request ~action:"merge" ~post_confirm_authority_lost:true
         ~phase:(F.Post_confirm { locked_mode = F.User })
         ())
  in
  ignore (expect_deny ~code:"post_confirm_authority_lost" d)

(* -------------------------------------------------------------------------- *)
(* Actor mode lock on retry                                                    *)
(* -------------------------------------------------------------------------- *)

let test_retry_keeps_user_mode () =
  let d =
    F.resolve
      (F.default_request ~action:"comment"
         ~phase:(F.Retry { locked_mode = F.User })
         ~user_path_available:true ~preview_actor:F.Names_app
         (* preview naming App must not switch locked User → App *) ())
  in
  ignore (expect_allow ~mode:F.User ~used_app_fallback:false d)

let test_retry_cannot_switch_user_to_app_when_user_lost () =
  let d =
    F.resolve
      (F.default_request ~action:"comment"
         ~phase:(F.Retry { locked_mode = F.User })
         ~user_path_available:false ~preview_actor:F.Names_app
         ~app_path_available:true ())
  in
  ignore (expect_deny ~code:"locked_user_path_unavailable" ~kind:F.Repair d)

let test_retry_keeps_app_fallback_mode () =
  let d =
    F.resolve
      (F.default_request ~action:"comment"
         ~phase:(F.Retry { locked_mode = F.App })
         ~preview_actor:F.Names_app ~app_path_available:true
         ~user_path_available:true ())
  in
  ignore (expect_allow ~mode:F.App ~used_app_fallback:true d)

let test_retry_app_requires_preview_still_names_app () =
  let d =
    F.resolve
      (F.default_request ~action:"comment"
         ~phase:(F.Retry { locked_mode = F.App })
         ~preview_actor:F.Names_user ~app_path_available:true ())
  in
  ignore
    (expect_deny ~code:"app_fallback_not_previewed" ~kind:F.Reconfirmation d)

let test_retry_user_required_locked_app_forbidden () =
  let d =
    F.resolve
      (F.default_request ~action:"merge"
         ~phase:(F.Retry { locked_mode = F.App })
         ~preview_actor:F.Names_app ~app_path_available:true ())
  in
  ignore (expect_deny ~code:"app_fallback_not_permitted" d)

(* -------------------------------------------------------------------------- *)
(* Policy helper + JSON                                                        *)
(* -------------------------------------------------------------------------- *)

let test_permits_app_fallback_helper () =
  Alcotest.(check bool)
    "preferred" true
    (Policy.permits_app_fallback Policy.User_preferred);
  Alcotest.(check bool)
    "required" false
    (Policy.permits_app_fallback Policy.User_required);
  Alcotest.(check bool)
    "app" false
    (Policy.permits_app_fallback Policy.App_installation);
  Alcotest.(check bool)
    "pat" false
    (Policy.permits_app_fallback Policy.Pat_compat)

let test_decision_json_no_secrets () =
  let d =
    F.resolve (F.default_request ~action:"merge" ~user_path_available:true ())
  in
  let blob = Yojson.Safe.to_string (F.decision_to_json d) in
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        ("no " ^ needle) false
        (let n = String.length needle in
         let len = String.length blob in
         let rec loop i =
           if i + n > len then false
           else if String.sub blob i n = needle then true
           else loop (i + 1)
         in
         loop 0))
    [ "ghu_"; "ghr_"; "Bearer "; "access_token" ]

let suite =
  [
    Alcotest.test_case "User_preferred uses user path" `Quick
      test_user_preferred_user_path;
    Alcotest.test_case
      "User_preferred visible App fallback when preview names App" `Quick
      test_user_preferred_visible_app_fallback;
    Alcotest.test_case "User_preferred no silent App without preview name"
      `Quick test_user_preferred_app_fallback_requires_preview;
    Alcotest.test_case "User_preferred never PAT fallback" `Quick
      test_user_preferred_never_pat_fallback;
    Alcotest.test_case "User_preferred App fallback needs App path" `Quick
      test_user_preferred_app_path_must_be_available;
    Alcotest.test_case "User_required allows user" `Quick
      test_user_required_allows_user;
    Alcotest.test_case "User_required never App even if preview names App"
      `Quick test_user_required_never_app_even_if_preview_names_app;
    Alcotest.test_case "User_required never PAT" `Quick
      test_user_required_never_pat;
    Alcotest.test_case "User_required no silent App when user missing" `Quick
      test_user_required_no_silent_app_when_user_missing;
    Alcotest.test_case "gate disabled blocks User_required" `Quick
      test_gate_disabled_blocks_user_required;
    Alcotest.test_case "gate disabled blocks User_preferred App fallback" `Quick
      test_gate_disabled_blocks_user_preferred_no_app_fallback;
    Alcotest.test_case "gate disabled allows pure App_installation" `Quick
      test_gate_disabled_allows_pure_app_installation;
    Alcotest.test_case "post-confirm authority loss no fallback" `Quick
      test_post_confirm_authority_loss_no_fallback;
    Alcotest.test_case "post-confirm authority loss User_required" `Quick
      test_post_confirm_authority_loss_user_required;
    Alcotest.test_case "retry keeps User mode" `Quick test_retry_keeps_user_mode;
    Alcotest.test_case "retry cannot switch User to App when user lost" `Quick
      test_retry_cannot_switch_user_to_app_when_user_lost;
    Alcotest.test_case "retry keeps App fallback mode" `Quick
      test_retry_keeps_app_fallback_mode;
    Alcotest.test_case "retry App still requires preview names App" `Quick
      test_retry_app_requires_preview_still_names_app;
    Alcotest.test_case "retry User_required locked App forbidden" `Quick
      test_retry_user_required_locked_app_forbidden;
    Alcotest.test_case "permits_app_fallback helper" `Quick
      test_permits_app_fallback_helper;
    Alcotest.test_case "decision json has no secrets" `Quick
      test_decision_json_no_secrets;
  ]
