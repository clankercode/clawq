From Coq Require Import Extraction.
From Coq Require Import ExtrOcamlBasic.
From Coq Require Import ExtrOcamlNativeString.
From Coq Require Import ExtrOcamlNatInt.
Require Import Clawq.Cli.
Require Import Clawq.Config.
Require Import Clawq.PathSafety.

Extraction Language OCaml.
Extraction "src/extracted/clawq_core.ml"
  (* CLI *)
  Clawq.Cli.parse_command
  Clawq.Cli.dispatch
  (* Config: basic *)
  Clawq.Config.validate_config
  Clawq.Config.valid_weights
  Clawq.Config.default_config
  (* Config: extended (F5) *)
  Clawq.Config.valid_port
  Clawq.Config.valid_temperature
  Clawq.Config.validate_config_full
  (* Path safety (F2) *)
  Clawq.PathSafety.norm_acc
  Clawq.PathSafety.normalize
  Clawq.PathSafety.is_prefix
  Clawq.PathSafety.is_path_safe_segs.
