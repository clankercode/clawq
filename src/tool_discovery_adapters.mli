(** OpenAI / Anthropic deferred-discovery adapters over frozen catalogs
    (P19.M1.E2.T005).

    Capability-aware per-vendor adapters preserve the frozen authorized catalog
    and canonical identity, use native behavior where supported, and fall back
    portably otherwise.

    - OpenAI: request fixtures exclude unselected client-search / deferred
      schemas (eager + portable search path).
    - Anthropic: may receive only authorized deferred definitions; never denied
      ones. *)

type vendor = OpenAI | Anthropic | Generic

val adapt : vendor -> catalog:Tool_catalog.t -> Yojson.Safe.t
(** Build the provider tools JSON for this vendor from a frozen catalog. *)

val openai_excludes_unselected_deferred : catalog:Tool_catalog.t -> bool
(** True when the OpenAI payload does not contain deferred tool names. *)

val anthropic_includes_only_authorized :
  catalog:Tool_catalog.t -> denied_names:string list -> bool
(** True when no [denied_names] appear in the Anthropic payload. *)
