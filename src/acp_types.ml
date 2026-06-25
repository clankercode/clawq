type stop_reason =
  | End_turn
  | Max_tokens
  | Max_turn_requests
  | Refusal
  | Cancelled

type tool_kind =
  | Read
  | Edit
  | Delete
  | Move
  | Search
  | Execute
  | Think
  | Fetch
  | Other

type tool_call_status = Pending | In_progress | Completed | Failed
type plan_entry_status = Plan_pending | Plan_in_progress | Plan_completed
type plan_entry_priority = High | Medium | Low

type permission_option_kind =
  | Allow_once
  | Allow_always
  | Reject_once
  | Reject_always

type implementation = {
  name : string;
  title : string option;
  version : string option;
}

type fs_capabilities = { read_text_file : bool; write_text_file : bool }
type client_capabilities = { fs : fs_capabilities; terminal : bool }

type prompt_capabilities = {
  image : bool;
  audio : bool;
  embedded_context : bool;
}

type mcp_capabilities = { http : bool; sse : bool }

type agent_capabilities = {
  load_session : bool;
  prompt_capabilities : prompt_capabilities option;
  mcp_capabilities : mcp_capabilities option;
}

type content_block =
  | Text of string
  | Image of { mime_type : string; data : string }
  | Audio of { mime_type : string; data : string }
  | Resource of { uri : string; mime_type : string option; text : string }
  | Resource_link of {
      uri : string;
      name : string;
      mime_type : string option;
      size : int option;
    }

type tool_call_location = { path : string; line : int option }

type tool_call_content =
  | Content_block of content_block
  | Diff of { path : string; old_text : string option; new_text : string }
  | Terminal of string

type tool_call = {
  tool_call_id : string;
  title : string;
  kind : tool_kind;
  status : tool_call_status;
  content : tool_call_content list;
  locations : tool_call_location list;
  raw_input : Yojson.Safe.t option;
  raw_output : Yojson.Safe.t option;
}

type tool_call_update = {
  tcu_tool_call_id : string;
  tcu_status : tool_call_status option;
  tcu_title : string option;
  tcu_content : tool_call_content list option;
  tcu_locations : tool_call_location list option;
  tcu_raw_input : Yojson.Safe.t option;
  tcu_raw_output : Yojson.Safe.t option;
}

type plan_entry = {
  pe_content : string;
  pe_priority : plan_entry_priority;
  pe_status : plan_entry_status;
}

type permission_option = {
  option_id : string;
  po_name : string;
  po_kind : permission_option_kind;
}

type session_update =
  | User_message_chunk of content_block
  | Agent_message_chunk of content_block
  | Thought_message_chunk of content_block
  | Tool_call of tool_call
  | Tool_call_update of tool_call_update
  | Plan of plan_entry list
  | Available_commands_update of Yojson.Safe.t
  | Current_mode_update of Yojson.Safe.t
  | Config_option_update of Yojson.Safe.t
  | Session_info_update of Yojson.Safe.t
  | Unknown of string

type mcp_server_config = {
  mcp_name : string;
  mcp_command : string;
  mcp_args : string list;
  mcp_env : (string * string) list;
}

(* --- JSON encoding --- *)

let string_of_stop_reason = function
  | End_turn -> "end_turn"
  | Max_tokens -> "max_tokens"
  | Max_turn_requests -> "max_turn_requests"
  | Refusal -> "refusal"
  | Cancelled -> "cancelled"

let stop_reason_of_string = function
  | "end_turn" -> End_turn
  | "max_tokens" -> Max_tokens
  | "max_turn_requests" -> Max_turn_requests
  | "refusal" -> Refusal
  | "cancelled" -> Cancelled
  | _ -> End_turn

let string_of_tool_kind = function
  | Read -> "read"
  | Edit -> "edit"
  | Delete -> "delete"
  | Move -> "move"
  | Search -> "search"
  | Execute -> "execute"
  | Think -> "think"
  | Fetch -> "fetch"
  | Other -> "other"

let tool_kind_of_string = function
  | "read" -> Read
  | "edit" -> Edit
  | "delete" -> Delete
  | "move" -> Move
  | "search" -> Search
  | "execute" -> Execute
  | "think" -> Think
  | "fetch" -> Fetch
  | _ -> Other

let string_of_tool_call_status = function
  | Pending -> "pending"
  | In_progress -> "in_progress"
  | Completed -> "completed"
  | Failed -> "failed"

let tool_call_status_of_string = function
  | "pending" -> Pending
  | "in_progress" -> In_progress
  | "completed" -> Completed
  | "failed" -> Failed
  | _ -> Pending

let string_of_plan_entry_status = function
  | Plan_pending -> "pending"
  | Plan_in_progress -> "in_progress"
  | Plan_completed -> "completed"

let plan_entry_status_of_string = function
  | "pending" -> Plan_pending
  | "in_progress" -> Plan_in_progress
  | "completed" -> Plan_completed
  | _ -> Plan_pending

let string_of_plan_entry_priority = function
  | High -> "high"
  | Medium -> "medium"
  | Low -> "low"

let plan_entry_priority_of_string = function
  | "high" -> High
  | "medium" -> Medium
  | "low" -> Low
  | _ -> Medium

let string_of_permission_option_kind = function
  | Allow_once -> "allow_once"
  | Allow_always -> "allow_always"
  | Reject_once -> "reject_once"
  | Reject_always -> "reject_always"

let permission_option_kind_of_string = function
  | "allow_once" -> Allow_once
  | "allow_always" -> Allow_always
  | "reject_once" -> Reject_once
  | "reject_always" -> Reject_always
  | _ -> Allow_once

let implementation_to_json impl =
  `Assoc
    ([ ("name", `String impl.name) ]
    @ (match impl.title with Some t -> [ ("title", `String t) ] | None -> [])
    @
    match impl.version with
    | Some v -> [ ("version", `String v) ]
    | None -> [])

let implementation_of_json json =
  let open Yojson.Safe.Util in
  {
    name = (try json |> member "name" |> to_string with _ -> "");
    title = (try Some (json |> member "title" |> to_string) with _ -> None);
    version =
      (try Some (json |> member "version" |> to_string) with _ -> None);
  }

let client_capabilities_to_json caps =
  `Assoc
    [
      ( "fs",
        `Assoc
          [
            ("readTextFile", `Bool caps.fs.read_text_file);
            ("writeTextFile", `Bool caps.fs.write_text_file);
          ] );
      ("terminal", `Bool caps.terminal);
    ]

let agent_capabilities_of_json json =
  let open Yojson.Safe.Util in
  {
    load_session =
      (try json |> member "loadSession" |> to_bool with _ -> false);
    prompt_capabilities =
      (try
         let pc = json |> member "promptCapabilities" in
         Some
           {
             image = (try pc |> member "image" |> to_bool with _ -> false);
             audio = (try pc |> member "audio" |> to_bool with _ -> false);
             embedded_context =
               (try pc |> member "embeddedContext" |> to_bool with _ -> false);
           }
       with _ -> None);
    mcp_capabilities =
      (try
         let mc = json |> member "mcpCapabilities" in
         Some
           {
             http = (try mc |> member "http" |> to_bool with _ -> false);
             sse = (try mc |> member "sse" |> to_bool with _ -> false);
           }
       with _ -> None);
  }

let content_block_to_json = function
  | Text s -> `Assoc [ ("type", `String "text"); ("text", `String s) ]
  | Image { mime_type; data } ->
      `Assoc
        [
          ("type", `String "image");
          ("mimeType", `String mime_type);
          ("data", `String data);
        ]
  | Audio { mime_type; data } ->
      `Assoc
        [
          ("type", `String "audio");
          ("mimeType", `String mime_type);
          ("data", `String data);
        ]
  | Resource { uri; mime_type; text } ->
      `Assoc
        ([ ("type", `String "resource") ]
        @ [
            ( "resource",
              `Assoc
                ([ ("uri", `String uri); ("text", `String text) ]
                @
                match mime_type with
                | Some mt -> [ ("mimeType", `String mt) ]
                | None -> []) );
          ])
  | Resource_link { uri; name; mime_type; size } ->
      `Assoc
        ([
           ("type", `String "resource_link");
           ("uri", `String uri);
           ("name", `String name);
         ]
        @ (match mime_type with
          | Some mt -> [ ("mimeType", `String mt) ]
          | None -> [])
        @ match size with Some s -> [ ("size", `Int s) ] | None -> [])

let content_block_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string with
  | "text" -> Text (json |> member "text" |> to_string)
  | "image" ->
      Image
        {
          mime_type = json |> member "mimeType" |> to_string;
          data = json |> member "data" |> to_string;
        }
  | "audio" ->
      Audio
        {
          mime_type = json |> member "mimeType" |> to_string;
          data = json |> member "data" |> to_string;
        }
  | "resource" ->
      let res = json |> member "resource" in
      Resource
        {
          uri = res |> member "uri" |> to_string;
          mime_type =
            (try Some (res |> member "mimeType" |> to_string) with _ -> None);
          text = (try res |> member "text" |> to_string with _ -> "");
        }
  | "resource_link" ->
      Resource_link
        {
          uri = json |> member "uri" |> to_string;
          name = json |> member "name" |> to_string;
          mime_type =
            (try Some (json |> member "mimeType" |> to_string) with _ -> None);
          size = (try Some (json |> member "size" |> to_int) with _ -> None);
        }
  | _ -> Text ""

let text_of_content_block = function
  | Text s -> s
  | Image _ -> "[image]"
  | Audio _ -> "[audio]"
  | Resource { uri; _ } -> Printf.sprintf "[resource: %s]" uri
  | Resource_link { name; _ } -> Printf.sprintf "[link: %s]" name

let tool_call_content_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string with
  | "content" ->
      Content_block (content_block_of_json (json |> member "content"))
  | "diff" ->
      Diff
        {
          path = json |> member "path" |> to_string;
          old_text =
            (try Some (json |> member "oldText" |> to_string) with _ -> None);
          new_text = json |> member "newText" |> to_string;
        }
  | "terminal" -> Terminal (json |> member "terminalId" |> to_string)
  | _ -> Content_block (Text "")

let tool_call_location_of_json json =
  let open Yojson.Safe.Util in
  {
    path = json |> member "path" |> to_string;
    line = (try Some (json |> member "line" |> to_int) with _ -> None);
  }

let tool_call_of_json json =
  let open Yojson.Safe.Util in
  {
    tool_call_id = json |> member "toolCallId" |> to_string;
    title = (try json |> member "title" |> to_string with _ -> "");
    kind =
      (try json |> member "kind" |> to_string |> tool_kind_of_string
       with _ -> Other);
    status =
      (try json |> member "status" |> to_string |> tool_call_status_of_string
       with _ -> Pending);
    content =
      (try
         json |> member "content" |> to_list
         |> List.map tool_call_content_of_json
       with _ -> []);
    locations =
      (try
         json |> member "locations" |> to_list
         |> List.map tool_call_location_of_json
       with _ -> []);
    raw_input =
      (try match json |> member "rawInput" with `Null -> None | j -> Some j
       with _ -> None);
    raw_output =
      (try match json |> member "rawOutput" with `Null -> None | j -> Some j
       with _ -> None);
  }

let tool_call_update_of_json json =
  let open Yojson.Safe.Util in
  {
    tcu_tool_call_id = json |> member "toolCallId" |> to_string;
    tcu_status =
      (try
         Some
           (json |> member "status" |> to_string |> tool_call_status_of_string)
       with _ -> None);
    tcu_title =
      (try Some (json |> member "title" |> to_string) with _ -> None);
    tcu_content =
      (try
         Some
           (json |> member "content" |> to_list
           |> List.map tool_call_content_of_json)
       with _ -> None);
    tcu_locations =
      (try
         Some
           (json |> member "locations" |> to_list
           |> List.map tool_call_location_of_json)
       with _ -> None);
    tcu_raw_input =
      (try match json |> member "rawInput" with `Null -> None | j -> Some j
       with _ -> None);
    tcu_raw_output =
      (try match json |> member "rawOutput" with `Null -> None | j -> Some j
       with _ -> None);
  }

let plan_entry_of_json json =
  let open Yojson.Safe.Util in
  {
    pe_content = json |> member "content" |> to_string;
    pe_priority =
      (try
         json |> member "priority" |> to_string |> plan_entry_priority_of_string
       with _ -> Medium);
    pe_status =
      (try json |> member "status" |> to_string |> plan_entry_status_of_string
       with _ -> Plan_pending);
  }

let permission_option_of_json json =
  let open Yojson.Safe.Util in
  {
    option_id = json |> member "optionId" |> to_string;
    po_name = (try json |> member "name" |> to_string with _ -> "");
    po_kind =
      (try
         json |> member "kind" |> to_string |> permission_option_kind_of_string
       with _ -> Allow_once);
  }

let session_update_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "sessionUpdate" |> to_string with
  | "user_message_chunk" ->
      User_message_chunk (content_block_of_json (json |> member "content"))
  | "agent_message_chunk" ->
      Agent_message_chunk (content_block_of_json (json |> member "content"))
  | "thought_message_chunk" ->
      Thought_message_chunk (content_block_of_json (json |> member "content"))
  | "tool_call" -> Tool_call (tool_call_of_json json)
  | "tool_call_update" -> Tool_call_update (tool_call_update_of_json json)
  | "plan" ->
      Plan (json |> member "entries" |> to_list |> List.map plan_entry_of_json)
  | "available_commands_update" -> Available_commands_update json
  | "current_mode_update" -> Current_mode_update json
  | "config_option_update" -> Config_option_update json
  | "session_info_update" -> Session_info_update json
  | s -> Unknown s

let string_of_session_update_type = function
  | User_message_chunk _ -> "user_message_chunk"
  | Agent_message_chunk _ -> "agent_message_chunk"
  | Thought_message_chunk _ -> "thought_message_chunk"
  | Tool_call _ -> "tool_call"
  | Tool_call_update _ -> "tool_call_update"
  | Plan _ -> "plan"
  | Available_commands_update _ -> "available_commands_update"
  | Current_mode_update _ -> "current_mode_update"
  | Config_option_update _ -> "config_option_update"
  | Session_info_update _ -> "session_info_update"
  | Unknown s -> Printf.sprintf "unknown(%s)" s

let mcp_server_config_to_json cfg =
  `Assoc
    [
      ("name", `String cfg.mcp_name);
      ("command", `String cfg.mcp_command);
      ("args", `List (List.map (fun s -> `String s) cfg.mcp_args));
      ( "env",
        `List
          (List.map
             (fun (n, v) ->
               `Assoc [ ("name", `String n); ("value", `String v) ])
             cfg.mcp_env) );
    ]
