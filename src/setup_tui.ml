(* setup_tui.ml -- Shared TUI abstraction for setup wizards *)

type field_kind =
  | Str
  | Secret
  | Bool
  | Int
  | Float
  | StrList
  | Choice of string list

type field = {
  key : string;
  label : string;
  menu_label : string;
  kind : field_kind;
  value : string ref;
  is_secret : bool;
  validate : string -> (string, string) result;
  description : string;
}

type wizard_spec = {
  title : string;
  docs_url : string;
  fields : field list;
  extra_actions : (string * string * (unit -> unit)) list;
  build_json : unit -> Yojson.Safe.t;
  pre_save_check : unit -> (unit, string) result;
  post_instructions : unit -> string;
}

let no_validate s = Ok s

let make_field ~key ~label ~menu_label ?(description = "")
    ?(validate = no_validate) ?(default = "") () =
  {
    key;
    label;
    menu_label;
    kind = Str;
    value = ref default;
    is_secret = false;
    validate;
    description;
  }

let make_secret_field ~key ~label ~menu_label ?(description = "")
    ?(validate = no_validate) ?(default = "") () =
  {
    key;
    label;
    menu_label;
    kind = Secret;
    value = ref default;
    is_secret = true;
    validate;
    description;
  }

let make_bool_field ~key ~label ~menu_label ?(description = "")
    ?(default = false) () =
  {
    key;
    label;
    menu_label;
    kind = Bool;
    value = ref (string_of_bool default);
    is_secret = false;
    validate = no_validate;
    description;
  }

let make_int_field ~key ~label ~menu_label ?(description = "")
    ?(validate = no_validate) ?(default = 0) () =
  {
    key;
    label;
    menu_label;
    kind = Int;
    value = ref (string_of_int default);
    is_secret = false;
    validate;
    description;
  }

let make_float_field ~key ~label ~menu_label ?(description = "")
    ?(validate = no_validate) ?(default = 0.0) () =
  {
    key;
    label;
    menu_label;
    kind = Float;
    value = ref (string_of_float default);
    is_secret = false;
    validate;
    description;
  }

let make_list_field ~key ~label ~menu_label ?(description = "")
    ?(validate = no_validate) ?(default = []) () =
  {
    key;
    label;
    menu_label;
    kind = StrList;
    value = ref (String.concat "," default);
    is_secret = false;
    validate;
    description;
  }

let make_choice_field ~key ~label ~menu_label ~choices ?(description = "")
    ?(validate = no_validate) ?(default = "") () =
  {
    key;
    label;
    menu_label;
    kind = Choice choices;
    value = ref default;
    is_secret = false;
    validate;
    description;
  }

(* Field value accessors *)

let get_str f = !(f.value)

let get_bool f =
  match String.lowercase_ascii !(f.value) with
  | "true" | "1" | "yes" -> true
  | _ -> false

let get_int f = try int_of_string !(f.value) with _ -> 0
let get_float f = try float_of_string !(f.value) with _ -> 0.0

let get_str_list f =
  let s = !(f.value) in
  if s = "" then []
  else
    String.split_on_char ',' s |> List.map String.trim
    |> List.filter (fun s -> s <> "")

let set_str_list f lst = f.value := String.concat "," lst

(* Display *)

let field_display_value f =
  let open Setup_common in
  let v = !(f.value) in
  if v = "" then dim "(not set)"
  else if f.is_secret then green (Tui_input.redact v)
  else
    match f.kind with
    | Bool ->
        if String.lowercase_ascii v = "true" then green "true" else dim "false"
    | _ -> green v

let draw_wizard_dashboard spec =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  let label_width =
    List.fold_left (fun acc f -> max acc (String.length f.label)) 0 spec.fields
    + 2
  in
  let lines =
    [ bold (Printf.sprintf " %s " spec.title); "" ]
    @ List.map
        (fun f ->
          Printf.sprintf "  %s  %s"
            (pad_right (f.label ^ ":") label_width)
            (field_display_value f))
        spec.fields
    @ [ "" ]
  in
  draw_box ~width:w lines;
  print_docs_link spec.docs_url;
  Printf.printf "\n";
  draw_separator ~width:w

let prompt_for_field f =
  let open Setup_common in
  Printf.printf "\n";
  if f.description <> "" then Printf.printf "  %s\n\n" (dim f.description);
  match f.kind with
  | Bool ->
      let current = get_bool f in
      let result = prompt_yn ~prompt:f.label ~default:current () in
      let new_val = string_of_bool result in
      if new_val <> !(f.value) then (
        f.value := new_val;
        true)
      else false
  | Secret -> (
      if !(f.value) <> "" then (
        Printf.printf "  Current: %s\n\n" (green (Tui_input.redact !(f.value)));
        let change = prompt_yn ~prompt:"Change?" ~default:false () in
        if not change then false
        else
          match prompt_secret ~prompt:f.label () with
          | Ok s -> (
              match f.validate s with
              | Ok v ->
                  f.value := v;
                  true
              | Error e ->
                  print_error e;
                  false)
          | Error e ->
              print_error e;
              false)
      else
        match prompt_secret ~prompt:f.label () with
        | Ok s -> (
            match f.validate s with
            | Ok v ->
                f.value := v;
                true
            | Error e ->
                print_error e;
                false)
        | Error e ->
            print_error e;
            false)
  | StrList -> (
      let default = !(f.value) in
      let input = prompt_string ~prompt:f.label ~default () in
      let lst =
        String.split_on_char ',' input
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in
      let new_val = String.concat "," lst in
      match f.validate new_val with
      | Ok v ->
          if v <> !(f.value) then (
            f.value := v;
            true)
          else false
      | Error e ->
          print_error e;
          false)
  | Choice choices ->
      Printf.printf "  Options: %s\n\n"
        (String.concat ", "
           (List.map (fun c -> if c = !(f.value) then green c else c) choices));
      let default = !(f.value) in
      let input = prompt_string ~prompt:f.label ~default () in
      if List.mem input choices then (
        match f.validate input with
        | Ok v ->
            if v <> !(f.value) then (
              f.value := v;
              true)
            else false
        | Error e ->
            print_error e;
            false)
      else (
        print_error (Printf.sprintf "Invalid choice: %s" input);
        false)
  | Int -> (
      let default = !(f.value) in
      let input = prompt_string ~prompt:f.label ~default () in
      match int_of_string_opt input with
      | Some _ -> (
          match f.validate input with
          | Ok v ->
              if v <> !(f.value) then (
                f.value := v;
                true)
              else false
          | Error e ->
              print_error e;
              false)
      | None ->
          print_error "Please enter a valid integer.";
          false)
  | Float -> (
      let default = !(f.value) in
      let input = prompt_string ~prompt:f.label ~default () in
      match float_of_string_opt input with
      | Some _ -> (
          match f.validate input with
          | Ok v ->
              if v <> !(f.value) then (
                f.value := v;
                true)
              else false
          | Error e ->
              print_error e;
              false)
      | None ->
          print_error "Please enter a valid number.";
          false)
  | Str -> (
      let default = !(f.value) in
      let input = prompt_string ~prompt:f.label ~default () in
      match f.validate input with
      | Ok v ->
          if v <> !(f.value) then (
            f.value := v;
            true)
          else false
      | Error e ->
          print_error e;
          false)

let run_wizard spec =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let dirty = ref false in
      let quit = ref false in
      while not !quit do
        draw_wizard_dashboard spec;
        let options =
          List.map (fun f -> (f.key, f.menu_label)) spec.fields
          @ List.map (fun (k, l, _) -> (k, l)) spec.extra_actions
          @ [ ("h", "Show setup instructions") ]
          @
          if !dirty then [ ("s", Setup_common.bold "Save configuration") ]
          else []
        in
        let choice =
          Setup_common.prompt_menu ~title:"Actions" ~options
            ~shortcut_exit:"q/Enter" ()
        in
        let key = String.lowercase_ascii choice in
        (* Check field and extra_action keys first so they always take priority *)
        let field_match = List.find_opt (fun f -> f.key = key) spec.fields in
        let extra_match =
          List.find_opt (fun (k, _, _) -> k = key) spec.extra_actions
        in
        match (field_match, extra_match) with
        | Some f, _ ->
            if prompt_for_field f then dirty := true;
            Setup_common.press_enter_to_continue ()
        | _, Some (_, _, handler) ->
            handler ();
            Setup_common.press_enter_to_continue ()
        | None, None -> (
            match key with
            | "q" | "" ->
                if !dirty then begin
                  let save =
                    Setup_common.prompt_yn
                      ~prompt:"You have unsaved changes. Save before exiting?"
                      ~default:true ()
                  in
                  if save then
                    match spec.pre_save_check () with
                    | Error e ->
                        Setup_common.print_warning e;
                        Setup_common.press_enter_to_continue ()
                    | Ok () -> (
                        let json = spec.build_json () in
                        match Setup_common.merge_and_write_config json with
                        | Ok path ->
                            Setup_common.print_success
                              (Printf.sprintf "Saved to %s" path);
                            quit := true
                        | Error e ->
                            Setup_common.print_error
                              (Printf.sprintf "Failed to write config: %s" e);
                            Setup_common.press_enter_to_continue ())
                  else quit := true
                end
                else quit := true
            | "h" ->
                Printf.printf "%s" (spec.post_instructions ());
                Setup_common.press_enter_to_continue ()
            | "s" -> (
                if not !dirty then (
                  Setup_common.print_warning "No changes to save.";
                  Setup_common.press_enter_to_continue ())
                else
                  match spec.pre_save_check () with
                  | Error e ->
                      Setup_common.print_warning e;
                      Setup_common.press_enter_to_continue ()
                  | Ok () -> (
                      let json = spec.build_json () in
                      match Setup_common.merge_and_write_config json with
                      | Ok path ->
                          Setup_common.print_success
                            (Printf.sprintf "Saved to %s" path);
                          dirty := false;
                          Setup_common.press_enter_to_continue ()
                      | Error e ->
                          Setup_common.print_error
                            (Printf.sprintf "Failed to write config: %s" e);
                          Setup_common.press_enter_to_continue ()))
            | _ ->
                Setup_common.print_warning
                  (Printf.sprintf "Unknown option: %s" key);
                Setup_common.press_enter_to_continue ())
      done;
      if !dirty then "Exited with unsaved changes."
      else Printf.sprintf "%s setup complete." spec.title
