type session_activity = Active | Inactive | Any

type session_info = {
  session_key : string;
  channel : string option;
  channel_id : string option;
  turn : string option;
  response_sent_at : string option;
  last_active : string option;
  message_count : int;
  archived_epoch_count : int;
  keepalive_enabled : bool;
  heartbeat_enabled : bool;
  effective_cwd : string option;
}

type raw_message = {
  id : int;
  role : string;
  content : string;
  tool_call_id : string option;
  tool_name : string option;
  tool_calls_json : string option;
  provider_response_items_json : string option;
  thinking_content : string option;
  created_at : string;
}

type session_epoch = {
  epoch_id : int option;
  label : string;
  current : bool;
  message_count : int;
  first_message_at : string option;
  last_message_at : string option;
  recorded_at : string option;
}

type epoch_selector = Current | Archived of int

type history_search_result = {
  role : string;
  content : string;
  created_at : string;
  source : string;
}

type session_archive_info = {
  archive_id : int;
  session_key : string;
  archived_at : string;
  message_count : int;
  epoch_count : int;
  first_message_at : string option;
  last_message_at : string option;
}

type room_profile = {
  id : int;
  name : string;
  created_at : string;
  updated_at : string;
}

type room_profile_binding = {
  room_id : string;
  profile_id : int;
  created_at : string;
}

type memory_scope = {
  id : int;
  kind : string;
  key : string;
  profile_id : int option;
  parent_scope_id : int option;
  provenance : string;
  created_at : string;
  updated_at : string;
}

type scoped_memory = {
  id : int;
  scope_id : int;
  scope_kind : string;
  scope_key : string;
  content : string option;
  reference : string;
  provenance : string;
  created_at : string;
  updated_at : string;
  redacted_at : string option;
  redaction_reason : string option;
  redaction_metadata : string option;
}
