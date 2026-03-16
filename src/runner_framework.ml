type runner = Codex | Claude | Kimi | Gemini | Opencode | Cursor
type invocation = Fresh | Resume of string

type session_id_strategy =
  | Pre_generate_uuid
  | Parse_jsonl_field of string
  | Parse_log_regex of string
  | Index_based

type resume_mode = By_session_id of string | By_last

type runner_def = {
  runner : runner;
  binary : string;
  session_strategy : session_id_strategy;
  build_fresh_argv :
    model:string option ->
    prompt:string ->
    pre_session_id:string option ->
    string array;
  build_resume_argv :
    model:string option ->
    resume_mode:resume_mode ->
    prompt:string ->
    string array;
}

type command_result = {
  argv : string array;
  pre_generated_session_id : string option;
}

let generate_uuid () =
  let buf = Mirage_crypto_rng.generate 16 in
  let bytes = Bytes.of_string buf in
  Bytes.set bytes 6
    (Char.chr (Char.code (Bytes.get bytes 6) land 0x0f lor 0x40));
  Bytes.set bytes 8
    (Char.chr (Char.code (Bytes.get bytes 8) land 0x3f lor 0x80));
  let hex =
    let s = Bytes.to_string bytes in
    let buf = Buffer.create 32 in
    String.iter
      (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
      s;
    Buffer.contents buf
  in
  Printf.sprintf "%s-%s-%s-%s-%s" (String.sub hex 0 8) (String.sub hex 8 4)
    (String.sub hex 12 4) (String.sub hex 16 4) (String.sub hex 20 12)

let model_args flag = function
  | Some model when String.trim model <> "" -> [| flag; model |]
  | _ -> [||]

let runner_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "codex" -> Some Codex
  | "claude" -> Some Claude
  | "kimi" -> Some Kimi
  | "gemini" -> Some Gemini
  | "opencode" -> Some Opencode
  | "cursor" -> Some Cursor
  | _ -> None

let runner_def_of_runner (runner : runner) : runner_def =
  match runner with
  | Codex ->
      {
        runner = Codex;
        binary = "codex";
        session_strategy = Parse_jsonl_field "thread_id";
        build_fresh_argv =
          (fun ~model ~prompt ~pre_session_id:_ ->
            Array.concat
              [
                [| "codex"; "exec"; "--json" |];
                model_args "--model" model;
                [| "--dangerously-bypass-approvals-and-sandbox"; prompt |];
              ]);
        build_resume_argv =
          (fun ~model ~resume_mode ~prompt ->
            match resume_mode with
            | By_session_id sid ->
                Array.concat
                  [
                    [| "codex"; "exec"; "resume"; sid; "--json" |];
                    model_args "--model" model;
                    [| "--dangerously-bypass-approvals-and-sandbox"; prompt |];
                  ]
            | By_last ->
                Array.concat
                  [
                    [| "codex"; "exec"; "resume"; "--last"; "--json" |];
                    model_args "--model" model;
                    [| "--dangerously-bypass-approvals-and-sandbox"; prompt |];
                  ]);
      }
  | Claude ->
      {
        runner = Claude;
        binary = "claude";
        session_strategy = Pre_generate_uuid;
        build_fresh_argv =
          (fun ~model ~prompt ~pre_session_id ->
            Array.concat
              [
                [| "claude" |];
                (match pre_session_id with
                | Some sid -> [| "--session-id"; sid |]
                | None -> [||]);
                [| "-p" |];
                model_args "--model" model;
                [| "--dangerously-skip-permissions"; prompt |];
              ]);
        build_resume_argv =
          (fun ~model ~resume_mode ~prompt ->
            match resume_mode with
            | By_session_id sid ->
                Array.concat
                  [
                    [| "claude"; "--resume"; sid; "-p" |];
                    model_args "--model" model;
                    [| "--dangerously-skip-permissions"; prompt |];
                  ]
            | By_last ->
                Array.concat
                  [
                    [| "claude"; "-c"; "-p" |];
                    model_args "--model" model;
                    [| "--dangerously-skip-permissions"; prompt |];
                  ]);
      }
  | Kimi ->
      {
        runner = Kimi;
        binary = "kimi";
        session_strategy = Parse_log_regex "[Ss]ession[=: ]+\\([^ \t\n\r]+\\)";
        build_fresh_argv =
          (fun ~model ~prompt ~pre_session_id:_ ->
            Array.concat
              [
                [| "kimi"; "--print"; "--yolo" |];
                model_args "--model" model;
                [| "-p"; prompt |];
              ]);
        build_resume_argv =
          (fun ~model ~resume_mode ~prompt ->
            match resume_mode with
            | By_session_id sid ->
                Array.concat
                  [
                    [| "kimi"; "--session"; sid; "--print"; "--yolo" |];
                    model_args "--model" model;
                    [| "-p"; prompt |];
                  ]
            | By_last ->
                Array.concat
                  [
                    [| "kimi"; "--continue"; "--print"; "--yolo" |];
                    model_args "--model" model;
                    [| "-p"; prompt |];
                  ]);
      }
  | Gemini ->
      {
        runner = Gemini;
        binary = "gemini";
        session_strategy = Index_based;
        build_fresh_argv =
          (fun ~model ~prompt ~pre_session_id:_ ->
            Array.concat
              [
                [| "gemini"; "--yolo" |];
                model_args "--model" model;
                [| "--prompt"; prompt |];
              ]);
        build_resume_argv =
          (fun ~model ~resume_mode:_ ~prompt ->
            Array.concat
              [
                [| "gemini"; "--resume"; "latest"; "--yolo" |];
                model_args "--model" model;
                [| "--prompt"; prompt |];
              ]);
      }
  | Opencode ->
      {
        runner = Opencode;
        binary = "opencode";
        session_strategy = Parse_log_regex "session[=: ]+\\(ses_[^ \t\n\r]+\\)";
        build_fresh_argv =
          (fun ~model ~prompt ~pre_session_id:_ ->
            Array.concat
              [
                [| "opencode"; "run" |];
                model_args "--model" model;
                [| prompt |];
              ]);
        build_resume_argv =
          (fun ~model ~resume_mode ~prompt ->
            match resume_mode with
            | By_session_id sid ->
                Array.concat
                  [
                    [| "opencode"; "run"; "--session"; sid |];
                    model_args "--model" model;
                    [| prompt |];
                  ]
            | By_last ->
                Array.concat
                  [
                    [| "opencode"; "run"; "-c" |];
                    model_args "--model" model;
                    [| prompt |];
                  ]);
      }
  | Cursor ->
      {
        runner = Cursor;
        binary = "cursor-agent";
        session_strategy =
          Parse_log_regex "chat[A-Za-z]*[=: ]+\\([^ \t\n\r]+\\)";
        build_fresh_argv =
          (fun ~model ~prompt ~pre_session_id:_ ->
            Array.concat
              [
                [| "cursor-agent"; "--print"; "--yolo"; "--trust" |];
                model_args "--model" model;
                [| prompt |];
              ]);
        build_resume_argv =
          (fun ~model ~resume_mode ~prompt ->
            match resume_mode with
            | By_session_id sid ->
                Array.concat
                  [
                    [|
                      "cursor-agent";
                      "--resume";
                      sid;
                      "--print";
                      "--yolo";
                      "--trust";
                    |];
                    model_args "--model" model;
                    [| prompt |];
                  ]
            | By_last ->
                Array.concat
                  [
                    [|
                      "cursor-agent";
                      "--continue";
                      "--print";
                      "--yolo";
                      "--trust";
                    |];
                    model_args "--model" model;
                    [| prompt |];
                  ]);
      }

let acp_argv_of_runner (runner : runner) : string array =
  match runner with
  | Claude -> [| "claude"; "--acp" |]
  | Codex -> [| "codex"; "--acp" |]
  | Kimi -> [| "kimi"; "acp" |]
  | Gemini -> [| "gemini"; "--experimental-acp" |]
  | Opencode -> [| "opencode"; "acp" |]
  | Cursor -> [| "cursor-agent"; "acp" |]

let runner_supports_acp (_runner : runner) : bool = true

let pre_generate_session_id (def : runner_def) : string option =
  match def.session_strategy with
  | Pre_generate_uuid -> Some (generate_uuid ())
  | _ -> None

let extract_session_id_from_jsonl ~field content =
  let lines = String.split_on_char '\n' content in
  let rec search = function
    | [] -> None
    | line :: rest -> (
        let trimmed = String.trim line in
        if trimmed = "" then search rest
        else
          match Yojson.Safe.from_string trimmed with
          | json -> (
              match Yojson.Safe.Util.member field json with
              | `String s when s <> "" -> Some s
              | _ -> search rest)
          | exception _ -> search rest)
  in
  search lines

let extract_session_id_from_regex ~pattern content =
  try
    let re = Str.regexp pattern in
    ignore (Str.search_forward re content 0);
    let n_groups =
      let rec count i =
        match Str.matched_group i content with
        | _ -> count (i + 1)
        | exception (Not_found | Invalid_argument _) -> i - 1
      in
      count 1
    in
    if n_groups >= 1 then Some (Str.matched_group n_groups content)
    else Some (Str.matched_string content)
  with Not_found -> None

let extract_session_id (def : runner_def) (content : string) : string option =
  match def.session_strategy with
  | Pre_generate_uuid -> None
  | Parse_jsonl_field field -> extract_session_id_from_jsonl ~field content
  | Parse_log_regex pattern -> extract_session_id_from_regex ~pattern content
  | Index_based -> None

let resume_mode_of ~(runner_session_id : string option) : resume_mode =
  match runner_session_id with
  | Some sid when String.trim sid <> "" -> By_session_id sid
  | _ -> By_last

let build_command_for ~(model : string option) ~(prompt : string)
    ~(runner_session_id : string option) (def : runner_def)
    (invocation : invocation) : command_result =
  match invocation with
  | Fresh ->
      let pre_id = pre_generate_session_id def in
      let argv = def.build_fresh_argv ~model ~prompt ~pre_session_id:pre_id in
      { argv; pre_generated_session_id = pre_id }
  | Resume resume_prompt ->
      let mode = resume_mode_of ~runner_session_id in
      let argv =
        def.build_resume_argv ~model ~resume_mode:mode ~prompt:resume_prompt
      in
      { argv; pre_generated_session_id = None }
