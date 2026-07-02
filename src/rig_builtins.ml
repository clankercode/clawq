let briefing_install_prompt =
  {|You are setting up the "daily briefing" rig for the user. This rig creates
a daily news/weather/topic briefing delivered to the user's preferred channel.

## Step 1: Install an RSS feed reader

Try to install `sfeed` (a minimal POSIX RSS/Atom fetcher):
- Use bash to detect the package manager (pacman, apt, brew, nix, etc.)
- Install sfeed via the package manager
- If sfeed is unavailable, try these fallbacks in order:
  1. `newsboat` (widely packaged)
  2. Write a minimal Python RSS fetcher script to ~/.clawq/rss_fetch.py:
     (script that takes URLs, fetches RSS XML, outputs title|link|date lines)

Verify the tool works by running a test fetch.

## Step 2: Configure feed sources

Store the feed configuration in ~/.clawq/briefing_feeds.txt (one URL per line).
Start with these defaults unless the user has specified otherwise:
- https://news.ycombinator.com/rss  # Hacker News
- http://feeds.bbci.co.uk/news/world/rss.xml  # BBC World
- https://feeds.arstechnica.com/arstechnica/index  # Ars Technica

## Step 3: Note the delivery session id

The cron jobs themselves run on a persistent worker session (`cron:briefing`) and deliver their output via `send_to_session` back to *this* session — wherever the user invoked `/rig install briefing`. Read the runtime context for `Session id:` and record that value; you will store it as `delivery_session` in Step 6.

## Step 4: Create the daily briefing cron job

Use bash to run:
  clawq cron add briefing-daily cron:briefing "0 7 * * *" Invoke the briefing-daily skill: call use_skill with name="briefing-daily". The skill handles config loading, pre-flight validation, RSS fetch, topic search, weather, composition, and delivery via send_to_session. Do not perform briefing work outside the skill — orchestrate only through use_skill.

## Step 5: Create the hourly notable-events cron job

Use bash to run:
  clawq cron add briefing-hourly cron:briefing "0 * * * *" Invoke the briefing-hourly skill: call use_skill with name="briefing-hourly". The skill handles config loading, pre-flight validation, planned-query emission, sequential web_search, and delivery via send_to_session. Do not perform briefing work outside the skill — orchestrate only through use_skill.

## Step 6: Store rig state

Use memory_store to save the briefing rig configuration:
- Key: "rig:briefing:config"
- Value: JSON with feeds file path, topics, weather location, cron job names, RSS tool used, AND `delivery_session` set to the Session id captured in Step 3. The delivery_session field is required — the briefing skills will refuse to run without it.

## Step 7: Confirm

Summarize what was installed and configured. Mention:
- Which RSS tool was installed
- Feed count and file location
- Cron job schedules
- How to adjust: /rig adjust briefing
- How to remove: /rig remove briefing|}

let briefing_adjust_prompt =
  {|You are adjusting the "daily briefing" rig configuration.

## Step 0: Migrate legacy cron jobs (idempotent)

Existing briefing cron jobs may still contain the legacy inline prompt (B678) and may run on the user's DM session_key directly. Migrate them to the deterministic built-in skills running on the persistent `cron:briefing` worker session, with delivery via `send_to_session` (B680).

### 0a: Capture the delivery session

Read the runtime context for `Session id:` — that is *this* session (where /rig adjust briefing is being run). Save it as DELIVERY_SESSION for the next steps.

### 0b: Migrate each cron job

For each job (`briefing-hourly` first, then `briefing-daily`):

1. Run `clawq cron show <name>` via bash. If the output already shows `Session: cron:briefing` AND the prompt mentions `use_skill with name="<name>"`, skip — already migrated.
2. Otherwise, capture the existing schedule, then run:

   ```
   clawq cron remove <name>
   clawq cron add <name> cron:briefing "<original_schedule>" Invoke the <name> skill: call use_skill with name="<name>". The skill handles all orchestration and delivers via send_to_session. Do not perform briefing work outside the skill.
   ```

3. If a job does not exist (cron show fails), skip — the user may not have it installed.

### 0c: Backfill delivery_session in config

After the cron migration:

1. `memory_recall(query="rig:briefing:config")` to read the current config.
2. If the config has no `delivery_session` field, parse the JSON, add `"delivery_session": "<DELIVERY_SESSION from 0a>"`, and `memory_store` the updated value back. Skip if already present.

## Step 1: Recall current configuration

Use memory_recall with key "rig:briefing:config" to retrieve the current
briefing configuration (feeds, topics, weather location, cron schedules,
RSS tool).

## Step 2: Show current state

Display the current configuration to the user:
- Feed sources (from ~/.clawq/briefing_feeds.txt)
- Monitored topics
- Weather location (if configured)
- Cron schedules (run: clawq cron show briefing-daily; clawq cron show briefing-hourly)

## Step 3: Ask what to change

Ask the user what they'd like to adjust:
- Add/remove RSS feed sources
- Change monitored topics
- Set/change weather location
- Adjust daily briefing time (cron schedule)
- Adjust hourly check frequency

## Step 4: Apply changes

Update the relevant files and cron jobs based on user preferences:
- For feeds: update ~/.clawq/briefing_feeds.txt
- For schedules: use bash with clawq cron edit
- For topics/weather: update memory_store with key "rig:briefing:config"

## Step 5: Confirm

Summarize what was changed.|}

let briefing_remove_prompt =
  {|You are removing the "daily briefing" rig.

## Step 1: Remove cron jobs

Use bash to run:
  clawq cron remove briefing-daily
  clawq cron remove briefing-hourly

## Step 2: Clean up configuration files

Remove the briefing feeds file:
  rm -f ~/.clawq/briefing_feeds.txt

## Step 3: Clear memory

Use memory_forget with key "rig:briefing:config" to remove the stored
configuration.

## Step 4: Confirm

Tell the user the briefing rig has been fully removed. The RSS tool itself
is left installed as it may be useful for other purposes.|}

type builtin_entry = {
  name : string;
  description : string;
  version : string;
  install : string;
  adjust : string;
  remove : string;
}

let entries : builtin_entry list =
  [
    {
      name = "briefing";
      description = "Daily news/weather/topic briefing pipeline";
      version = "1.0";
      install = briefing_install_prompt;
      adjust = briefing_adjust_prompt;
      remove = briefing_remove_prompt;
    };
  ]
