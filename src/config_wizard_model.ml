(* config_wizard_model.ml — Types for the config wizard state machine *)

type wizard_mode = Onboard | FullWizard

type step =
  | Welcome
  | ProviderSelect
  | ProviderApiKey
  | ProviderBaseUrl
  | ProviderTestOffer
  | ProviderTestResult
  | ModelSelect
  | SecurityTools
  | SecurityWorkspace
  | ChannelMenu
  | ChannelTelegram
  | ChannelDiscord
  | ChannelSlack
  | GatewayConfig
  | MemoryConfig
  | Review
  | Confirm
  | Done

type msg =
  | KeyChar of char
  | KeyEnter
  | KeyBackspace
  | KeyUp
  | KeyDown
  | KeyEsc
  | KeyTab
  | ValidationResult of (string, string) result

type action =
  | Noop
  | WriteConfig
  | TestProvider of string * string * string
  | Quit

type text_input = {
  label : string;
  value : string;
  cursor : int;
  secret : bool;
  placeholder : string;
}

type select_input = { label : string; options : string list; selected : int }
type confirm_input = { label : string; value : bool }

type widget =
  | TextInput of text_input
  | Select of select_input
  | ConfirmInput of confirm_input

type provider_draft = {
  name : string;
  api_key : string;
  base_url : string;
  default_model : string;
}

type channel_selections = { telegram : bool; discord : bool; slack : bool }

type model = {
  mode : wizard_mode;
  step : step;
  widget : widget;
  providers : provider_draft list;
  current_provider : provider_draft;
  primary_model : string;
  tools_enabled : bool;
  workspace_only : bool;
  extra_paths : string list;
  channel_sel : channel_selections;
  telegram_token : string;
  discord_token : string;
  slack_bot_token : string;
  slack_app_token : string;
  slack_signing_secret : string;
  gateway_host : string;
  gateway_port : string;
  gateway_auth_token : string;
  messages : string list;
  history : step list;
  test_result : string option;
}

let empty_provider =
  { name = ""; api_key = ""; base_url = ""; default_model = "" }

let default_channel_sel = { telegram = false; discord = false; slack = false }

let make_text_input ?(secret = false) ?(placeholder = "") ?(value = "") label =
  TextInput { label; value; cursor = String.length value; secret; placeholder }

let make_select label options = Select { label; options; selected = 0 }
let make_confirm ?(value = true) label = ConfirmInput { label; value }

let initial_model mode =
  {
    mode;
    step = Welcome;
    widget = make_confirm "Ready to begin?";
    providers = [];
    current_provider = empty_provider;
    primary_model = "openai/gpt-4o";
    tools_enabled = true;
    workspace_only = true;
    extra_paths = [];
    channel_sel = default_channel_sel;
    telegram_token = "";
    discord_token = "";
    slack_bot_token = "";
    slack_app_token = "";
    slack_signing_secret = "";
    gateway_host = "127.0.0.1";
    gateway_port = "13451";
    gateway_auth_token = "";
    messages = [];
    history = [];
    test_result = None;
  }
