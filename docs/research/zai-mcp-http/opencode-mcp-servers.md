# OpenCode MCP Server Notes Relevant To Z.AI

Source: `https://opencode.ai/docs/mcp-servers`
Fetched: 2026-03-10

Relevant OpenCode config differences:

## Local MCP

```json
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "my-local-mcp-server": {
      "type": "local",
      "command": ["npx", "-y", "my-mcp-command"],
      "enabled": true,
      "environment": {
        "MY_ENV_VAR": "my_env_var_value"
      },
      "timeout": 5000
    }
  }
}
```

## Remote MCP

```json
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "my-remote-mcp": {
      "type": "remote",
      "url": "https://my-mcp-server.com",
      "enabled": true,
      "headers": {
        "Authorization": "Bearer MY_API_KEY"
      },
      "timeout": 5000
    }
  }
}
```

Important notes from OpenCode docs:

- Local servers start a process from `command`
- Remote servers do not start a local process; they use `url` plus optional `headers`
- Remote servers may also use OAuth, but API-key based servers can set `oauth: false`
- MCP tools are registered with the server name as prefix in OpenCode
