let briefing_install_prompt =
  {|You are setting up the "daily briefing" rig for the user. This rig creates
a daily news/weather/topic briefing delivered to the user's preferred channel.

## Step 1: Install an RSS feed reader

Try to install `sfeed` (a minimal POSIX RSS/Atom fetcher):
- Use shell_exec to detect the package manager (pacman, apt, brew, nix, etc.)
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

## Step 3: Create the daily briefing cron job

Use shell_exec to run:
  clawq cron add briefing-daily briefing "0 7 * * *" You are generating the user's daily briefing. 1. Run the installed RSS tool to fetch headlines from ~/.clawq/briefing_feeds.txt. Extract the top 5 most notable headlines. 2. Use web_search for each monitored topic to find recent developments. 3. If a weather location is configured, use web_search for today's weather. 4. Compose a briefing with sections: Top Headlines, Topic Updates, Weather, Worth Reading. Keep it 400-800 words, scannable, with bullet points and links.

## Step 4: Create the hourly notable-events cron job

Use shell_exec to run:
  clawq cron add briefing-hourly briefing "0 * * * *" Quick breaking-news check. Use web_search for each monitored topic. Add 'breaking' or 'just announced' qualifiers. If nothing genuinely notable happened, respond with EXACTLY: 'Nothing notable.' Only report truly significant events. 2-3 sentences with a link if notable.

## Step 5: Store rig state

Use memory_store to save the briefing rig configuration:
- Key: "rig:briefing:config"
- Value: JSON with feeds file path, topics, weather location, cron job names, RSS tool used

## Step 6: Confirm

Summarize what was installed and configured. Mention:
- Which RSS tool was installed
- Feed count and file location
- Cron job schedules
- How to adjust: /rig adjust briefing
- How to remove: /rig remove briefing|}

let briefing_adjust_prompt =
  {|You are adjusting the "daily briefing" rig configuration.

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
- For schedules: use shell_exec with clawq cron edit
- For topics/weather: update memory_store with key "rig:briefing:config"

## Step 5: Confirm

Summarize what was changed.|}

let briefing_remove_prompt =
  {|You are removing the "daily briefing" rig.

## Step 1: Remove cron jobs

Use shell_exec to run:
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
