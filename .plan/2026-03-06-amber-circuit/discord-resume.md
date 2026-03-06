# Discord RESUME Persistence

## Current State

Discord gateway tracks three pieces of resume state in `discord.ml` local refs:
- `resume_session_id : string option ref` (line 291)
- `resume_seq : int option ref` (line 292)
- `resume_url : string option ref` (line 293)

These are captured from the `discord_gateway.t` after disconnect (lines 325-327). They are
**in-memory only** — lost on daemon restart. Without them, Discord reconnects via IDENTIFY
(new session), which:
- Takes ~5 seconds longer (session setup)
- Misses any events dispatched while disconnected

With persisted RESUME state, the reconnect takes ~1-2 seconds and Discord replays missed
events since the last sequence number.

## DB Schema

`discord_resume_state` table added in schema migration to v2 (see session-persistence.md):

```sql
CREATE TABLE IF NOT EXISTS discord_resume_state (
  id                 INTEGER PRIMARY KEY CHECK (id = 1),
  session_id         TEXT NOT NULL,
  seq                INTEGER NOT NULL,
  resume_gateway_url TEXT NOT NULL,
  updated_at         TEXT NOT NULL DEFAULT (datetime('now'))
);
```

Singleton pattern (`id = 1`) — only one Discord connection per daemon instance.

## Write Path: discord.ml

After updating the in-memory refs (lines 325-327), also persist to DB:

```ocaml
(* After: resume_session_id := Discord_gateway.session_id gw; etc. *)
(match !resume_session_id, !resume_seq, !resume_url with
| Some sid, Some seq, Some url ->
  Discord_db.save_resume_state ~db ~session_id:sid ~seq ~resume_gateway_url:url
| _ -> ())
```

Also update on every `seq` increment (discord_gateway.ml:128) — but batching writes is better.
In practice, writing after disconnect (i.e., just before reconnect) is sufficient since the seq
is used for RESUME only on reconnect. Write on disconnect is the right moment.

## Read Path: discord.ml

At startup of the Discord channel (`Discord.start ~config ~db ...`), before entering the
reconnect loop, load persisted resume state:

```ocaml
let resume_session_id, resume_seq, resume_url =
  match Discord_db.load_resume_state ~db with
  | Some { session_id; seq; resume_gateway_url } ->
    Logs.info (fun m -> m "Discord: loaded resume state (seq=%d)" seq);
    ref (Some session_id), ref (Some seq), ref (Some resume_gateway_url)
  | None ->
    ref None, ref None, ref None
in
```

The existing reconnect logic at lines 343-363 already handles the RESUME vs IDENTIFY decision
based on whether `resume_session_id` is set — no changes needed there.

## New helper: Discord_db module (or inline in discord.ml)

```ocaml
(* src/discord_db.ml or inline in discord.ml *)

type resume_state = {
  session_id : string;
  seq : int;
  resume_gateway_url : string;
}

val save_resume_state :
  db:Sqlite3.db option ->
  session_id:string -> seq:int -> resume_gateway_url:string -> unit

val load_resume_state :
  db:Sqlite3.db option ->
  resume_state option
```

Implementation uses `INSERT OR REPLACE INTO discord_resume_state (id, ...) VALUES (1, ...)`.

## Notes

- The `db` parameter needs to be threaded through to `Discord.start`. Currently discord.ml
  receives config but not the DB handle. Adding `~db:Sqlite3.db option` to `Discord.start`
  is the right approach (consistent with how session.ml passes db).
- Fatal close codes (4004, 4010-4014) already clear resume refs in discord.ml:345-348.
  Add a DB clear there too: `Discord_db.clear_resume_state ~db`.
- Slack Socket Mode and Telegram do not have analogous resume state worth persisting.
