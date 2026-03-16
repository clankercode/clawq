(* test_setup_tui.ml -- Tests for Setup_tui shared TUI abstraction *)

let make_field_defaults () =
  let f =
    Setup_tui.make_field ~key:"t" ~label:"Test" ~menu_label:"Set test" ()
  in
  Alcotest.(check string) "key" "t" f.key;
  Alcotest.(check string) "label" "Test" f.label;
  Alcotest.(check string) "default empty" "" (Setup_tui.get_str f)

let make_field_with_default () =
  let f =
    Setup_tui.make_field ~key:"t" ~label:"Test" ~menu_label:"Set test"
      ~default:"hello" ()
  in
  Alcotest.(check string) "has default" "hello" (Setup_tui.get_str f)

let make_secret_field_is_secret () =
  let f =
    Setup_tui.make_secret_field ~key:"s" ~label:"Secret"
      ~menu_label:"Set secret" ()
  in
  Alcotest.(check bool) "is_secret" true f.is_secret

let make_bool_field_default_false () =
  let f =
    Setup_tui.make_bool_field ~key:"b" ~label:"Enabled" ~menu_label:"Toggle" ()
  in
  Alcotest.(check bool) "default false" false (Setup_tui.get_bool f)

let make_bool_field_default_true () =
  let f =
    Setup_tui.make_bool_field ~key:"b" ~label:"Enabled" ~menu_label:"Toggle"
      ~default:true ()
  in
  Alcotest.(check bool) "default true" true (Setup_tui.get_bool f)

let make_int_field_default () =
  let f =
    Setup_tui.make_int_field ~key:"p" ~label:"Port" ~menu_label:"Set port"
      ~default:8080 ()
  in
  Alcotest.(check int) "default 8080" 8080 (Setup_tui.get_int f)

let make_float_field_default () =
  let f =
    Setup_tui.make_float_field ~key:"i" ~label:"Interval"
      ~menu_label:"Set interval" ~default:5.0 ()
  in
  Alcotest.(check (float 0.01)) "default 5.0" 5.0 (Setup_tui.get_float f)

let make_list_field_default () =
  let f =
    Setup_tui.make_list_field ~key:"l" ~label:"List" ~menu_label:"Set list"
      ~default:[ "a"; "b"; "c" ] ()
  in
  Alcotest.(check (list string))
    "default list" [ "a"; "b"; "c" ] (Setup_tui.get_str_list f)

let make_list_field_empty () =
  let f =
    Setup_tui.make_list_field ~key:"l" ~label:"List" ~menu_label:"Set list" ()
  in
  Alcotest.(check (list string)) "empty list" [] (Setup_tui.get_str_list f)

let set_str_list_roundtrip () =
  let f =
    Setup_tui.make_list_field ~key:"l" ~label:"List" ~menu_label:"Set list" ()
  in
  Setup_tui.set_str_list f [ "x"; "y"; "z" ];
  Alcotest.(check (list string))
    "set list" [ "x"; "y"; "z" ] (Setup_tui.get_str_list f)

let get_bool_variants () =
  let f = Setup_tui.make_bool_field ~key:"b" ~label:"B" ~menu_label:"B" () in
  f.value := "true";
  Alcotest.(check bool) "true" true (Setup_tui.get_bool f);
  f.value := "1";
  Alcotest.(check bool) "1" true (Setup_tui.get_bool f);
  f.value := "yes";
  Alcotest.(check bool) "yes" true (Setup_tui.get_bool f);
  f.value := "false";
  Alcotest.(check bool) "false" false (Setup_tui.get_bool f);
  f.value := "no";
  Alcotest.(check bool) "no" false (Setup_tui.get_bool f);
  f.value := "garbage";
  Alcotest.(check bool) "garbage" false (Setup_tui.get_bool f)

let get_int_invalid () =
  let f =
    Setup_tui.make_int_field ~key:"p" ~label:"Port" ~menu_label:"Set port" ()
  in
  f.value := "not_a_number";
  Alcotest.(check int) "invalid returns 0" 0 (Setup_tui.get_int f)

let no_validate_always_ok () =
  Alcotest.(check (result string string))
    "ok" (Ok "anything")
    (Setup_tui.no_validate "anything")

let make_choice_field_default () =
  let f =
    Setup_tui.make_choice_field ~key:"m" ~label:"Mode" ~menu_label:"Set mode"
      ~choices:[ "a"; "b"; "c" ] ~default:"b" ()
  in
  Alcotest.(check string) "default b" "b" (Setup_tui.get_str f)

let suite =
  [
    Alcotest.test_case "make_field defaults" `Quick make_field_defaults;
    Alcotest.test_case "make_field with default" `Quick make_field_with_default;
    Alcotest.test_case "make_secret_field is_secret" `Quick
      make_secret_field_is_secret;
    Alcotest.test_case "make_bool_field default false" `Quick
      make_bool_field_default_false;
    Alcotest.test_case "make_bool_field default true" `Quick
      make_bool_field_default_true;
    Alcotest.test_case "make_int_field default" `Quick make_int_field_default;
    Alcotest.test_case "make_float_field default" `Quick
      make_float_field_default;
    Alcotest.test_case "make_list_field default" `Quick make_list_field_default;
    Alcotest.test_case "make_list_field empty" `Quick make_list_field_empty;
    Alcotest.test_case "set_str_list roundtrip" `Quick set_str_list_roundtrip;
    Alcotest.test_case "get_bool variants" `Quick get_bool_variants;
    Alcotest.test_case "get_int invalid" `Quick get_int_invalid;
    Alcotest.test_case "no_validate always ok" `Quick no_validate_always_ok;
    Alcotest.test_case "make_choice_field default" `Quick
      make_choice_field_default;
  ]
