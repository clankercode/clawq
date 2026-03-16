type edit_support = Edit_in_place | Delete_and_resend | No_edit

type t = {
  can_edit : edit_support;
  can_delete : bool;
  can_react : bool;
  can_type : bool;
  max_message_length : int;
  connector : Format_adapter.connector;
  parse_mode : string;
  debounce_interval : float;
}

let telegram =
  {
    can_edit = Edit_in_place;
    can_delete = true;
    can_react = false;
    can_type = true;
    max_message_length = 4096;
    connector = Format_adapter.Telegram_html;
    parse_mode = "HTML";
    debounce_interval = 0.5;
  }

let discord =
  {
    can_edit = Edit_in_place;
    can_delete = true;
    can_react = true;
    can_type = false;
    max_message_length = 2000;
    connector = Format_adapter.Discord;
    parse_mode = "Markdown";
    debounce_interval = 0.5;
  }

let slack =
  {
    can_edit = Edit_in_place;
    can_delete = true;
    can_react = true;
    can_type = false;
    max_message_length = 4000;
    connector = Format_adapter.Slack;
    parse_mode = "mrkdwn";
    debounce_interval = 0.5;
  }

let teams =
  {
    can_edit = Edit_in_place;
    can_delete = true;
    can_react = false;
    can_type = true;
    max_message_length = 28672;
    connector = Format_adapter.Teams;
    parse_mode = "Markdown";
    debounce_interval = 1.0;
  }

let matrix =
  {
    can_edit = Edit_in_place;
    can_delete = true;
    can_react = false;
    can_type = false;
    max_message_length = 4000;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.5;
  }

let irc =
  {
    can_edit = No_edit;
    can_delete = false;
    can_react = false;
    can_type = false;
    max_message_length = 512;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let mattermost =
  {
    can_edit = Edit_in_place;
    can_delete = true;
    can_react = true;
    can_type = false;
    max_message_length = 16383;
    connector = Format_adapter.Discord;
    parse_mode = "Markdown";
    debounce_interval = 0.5;
  }

let lark =
  {
    can_edit = No_edit;
    can_delete = false;
    can_react = false;
    can_type = false;
    max_message_length = 4096;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let line =
  {
    can_edit = No_edit;
    can_delete = false;
    can_react = false;
    can_type = false;
    max_message_length = 5000;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let dingtalk =
  {
    can_edit = No_edit;
    can_delete = false;
    can_react = false;
    can_type = false;
    max_message_length = 20000;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let onebot =
  {
    can_edit = No_edit;
    can_delete = true;
    can_react = false;
    can_type = false;
    max_message_length = 4500;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let nostr =
  {
    can_edit = No_edit;
    can_delete = false;
    can_react = false;
    can_type = false;
    max_message_length = 8000;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let imessage =
  {
    can_edit = No_edit;
    can_delete = false;
    can_react = false;
    can_type = false;
    max_message_length = 4096;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let email =
  {
    can_edit = No_edit;
    can_delete = false;
    can_react = false;
    can_type = false;
    max_message_length = 65536;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let github =
  {
    can_edit = Edit_in_place;
    can_delete = true;
    can_react = true;
    can_type = false;
    max_message_length = 65536;
    connector = Format_adapter.Discord;
    parse_mode = "Markdown";
    debounce_interval = 0.5;
  }

let signal =
  {
    can_edit = No_edit;
    can_delete = true;
    can_react = true;
    can_type = false;
    max_message_length = 6000;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let whatsapp =
  {
    can_edit = No_edit;
    can_delete = false;
    can_react = true;
    can_type = false;
    max_message_length = 4096;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let web_channel =
  {
    can_edit = No_edit;
    can_delete = false;
    can_react = false;
    can_type = false;
    max_message_length = 65536;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }

let plain =
  {
    can_edit = No_edit;
    can_delete = false;
    can_react = false;
    can_type = false;
    max_message_length = 4096;
    connector = Format_adapter.Plain;
    parse_mode = "Markdown";
    debounce_interval = 0.0;
  }
