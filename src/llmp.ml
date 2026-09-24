let provider_name = "llmp"
let anthropic_provider_name = "llmp_anthropic"
let mesh_provider_name = "llmp_mesh"
let mesh_anthropic_provider_name = "llmp_mesh_anthropic"
let default_base_url = "https://omni-dyn-00.amaroolabs.com/v1"
let anthropic_base_url = "https://omni-dyn-00.amaroolabs.com"
let mesh_base_url = "http://10.42.0.8:24584/v1"
let mesh_anthropic_base_url = "http://10.42.0.8:24584"
let key_filename = "llmp-api.key"
let api_key_env = "LLMP_API_KEY"
let key_command = "llmp-api-key"
let eq_name a b = String.lowercase_ascii a = String.lowercase_ascii b
let name_is name expected = eq_name name expected

let kind_is (p : Runtime_config.provider_config) expected =
  match p.kind with Some kind -> eq_name kind expected | None -> false

let is_openai_provider ~name (p : Runtime_config.provider_config) =
  name_is name provider_name
  || name_is name mesh_provider_name
  || kind_is p provider_name
  || kind_is p mesh_provider_name

let is_anthropic_provider ~name (p : Runtime_config.provider_config) =
  name_is name anthropic_provider_name
  || name_is name mesh_anthropic_provider_name
  || kind_is p anthropic_provider_name
  || kind_is p mesh_anthropic_provider_name

let is_provider ~name (p : Runtime_config.provider_config) =
  is_openai_provider ~name p || is_anthropic_provider ~name p

let trim_trailing_slashes url =
  let rec end_index i =
    if i > 0 && url.[i - 1] = '/' then end_index (i - 1) else i
  in
  String.sub url 0 (end_index (String.length url))

let urls_equal a b = eq_name (trim_trailing_slashes a) (trim_trailing_slashes b)

let anthropic_origin url =
  urls_equal url anthropic_base_url || urls_equal url mesh_anthropic_base_url

let is_anthropic_config (p : Runtime_config.provider_config) =
  kind_is p anthropic_provider_name
  || kind_is p mesh_anthropic_provider_name
  || match p.base_url with Some url -> anthropic_origin url | None -> false

let is_mesh ~name (p : Runtime_config.provider_config) =
  name_is name mesh_provider_name
  || name_is name mesh_anthropic_provider_name
  || kind_is p mesh_provider_name
  || kind_is p mesh_anthropic_provider_name
  ||
  match p.base_url with
  | Some url ->
      urls_equal url mesh_base_url || urls_equal url mesh_anthropic_base_url
  | None -> false

let base_url_for ~name (p : Runtime_config.provider_config) =
  match p.base_url with
  | Some url -> url
  | None ->
      if is_anthropic_provider ~name p then
        if is_mesh ~name p then mesh_anthropic_base_url else anthropic_base_url
      else if is_mesh ~name p then mesh_base_url
      else default_base_url

let cached_key : string option ref = ref None

let read_key_file path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let raw = really_input_string ic (in_channel_length ic) in
        let key = String.trim raw in
        if key = "" then None else Some key)
  with Sys_error _ -> None

let first_line s =
  match String.index_opt s '\n' with
  | None -> String.trim s
  | Some i -> String.trim (String.sub s 0 i)

let close_fd fd = try Unix.close fd with _ -> ()

let read_fd fd =
  let buf = Buffer.create 64 in
  let chunk = Bytes.create 256 in
  let rec read () =
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buf chunk 0 n;
        read ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> read ()
  in
  (try read () with _ -> ());
  close_fd fd;
  Buffer.contents buf

let read_key_command () =
  let argv = [| key_command |] in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  match Unix.openfile "/dev/null" [ Unix.O_WRONLY; Unix.O_CLOEXEC ] 0o0 with
  | exception Unix.Unix_error _ ->
      close_fd out_r;
      close_fd out_w;
      None
  | devnull -> (
      match Unix.create_process key_command argv Unix.stdin out_w devnull with
      | exception Unix.Unix_error _ ->
          close_fd out_r;
          close_fd out_w;
          close_fd devnull;
          None
      | pid -> (
          close_fd out_w;
          close_fd devnull;
          let stdout = read_fd out_r in
          let status =
            try snd (Unix.waitpid [] pid) with _ -> Unix.WEXITED 127
          in
          match status with
          | Unix.WEXITED 0 ->
              let key = first_line stdout in
              if key = "" then None else Some key
          | _ -> None))

let remember key =
  cached_key := Some key;
  key

let resolve_api_key configured =
  if Runtime_config.is_key_set configured then configured
  else
    match !cached_key with
    | Some key -> key
    | None -> (
        match read_key_file (Dot_dir.sub key_filename) with
        | Some key -> remember key
        | None -> (
            match Sys.getenv_opt api_key_env with
            | Some key when String.trim key <> "" -> String.trim key
            | _ -> (
                match read_key_command () with
                | Some key -> remember key
                | None -> "")))

let has_routable_auth configured = Runtime_config.is_key_set configured
let reset_key_cache () = cached_key := None

let models_uri ~name (p : Runtime_config.provider_config) =
  match p.base_url with
  | Some url when not (anthropic_origin url) ->
      trim_trailing_slashes url ^ "/models"
  | _ ->
      let base = if is_mesh ~name p then mesh_base_url else default_base_url in
      trim_trailing_slashes base ^ "/models"

(* Strip only an LLMP provider prefix. Model ids such as xai/grok-4.7 keep
   their namespace; a generic slash strip would turn that into grok-4.7. *)
let api_model_id model =
  let lower = String.lowercase_ascii model in
  let prefixes =
    [
      provider_name;
      anthropic_provider_name;
      mesh_provider_name;
      mesh_anthropic_provider_name;
    ]
  in
  let strip prefix =
    let take delim =
      let pref = prefix ^ delim in
      let n = String.length pref in
      if String.length lower >= n && String.sub lower 0 n = pref then
        Some (String.sub model n (String.length model - n))
      else None
    in
    match take ":" with Some id -> Some id | None -> take "/"
  in
  match List.find_map strip prefixes with Some id -> id | None -> model
