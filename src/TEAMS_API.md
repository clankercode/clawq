# MS Teams Bot Framework API Reference

Notes captured from the Microsoft Bot Framework documentation and the pag-server reference implementation. Used by `src/teams.ml`.

## Protocol Overview

Teams uses the **Microsoft Bot Framework** webhook protocol:
- Teams POSTs JSON `Activity` objects to your configured webhook URL.
- Your bot replies via REST API back to `serviceUrl` (per-activity field, not config).
- Auth is two-way: inbound requests carry a JWT Bearer token; outbound replies require an OAuth 2.0 bearer token.

## Inbound: Activity Schema

Teams POSTs to your webhook path with `Content-Type: application/json`:

```json
{
  "type": "message",
  "id": "<activity_id>",
  "timestamp": "2024-01-01T00:00:00.000Z",
  "serviceUrl": "https://smba.trafficmanager.net/amer/",
  "channelId": "msteams",
  "from": {
    "id": "<user_aad_object_id>",
    "name": "User Name",
    "aadObjectId": "<same_as_id>"
  },
  "conversation": {
    "id": "<conversation_id>",
    "isGroup": false,
    "tenantId": "<tenant_id>"
  },
  "recipient": {
    "id": "<bot_id>",
    "name": "Bot Name"
  },
  "text": "Hello bot",
  "channelData": {
    "team": { "id": "<team_id>", "name": "Team Name" },
    "channel": { "id": "<channel_id>", "name": "Channel Name" },
    "tenant": { "id": "<tenant_id>" }
  }
}
```

Key fields:
- `type` — usually `"message"` for user messages; also `"conversationUpdate"`, `"invoke"` etc. (ignore non-message types)
- `text` — the user's message text (may contain `<at>BotName</at>` mentions in group chats)
- `from.id` — user's AAD object ID (use for allow_users filtering)
- `conversation.id` — conversation ID (use in reply URL)
- `channelData.team.id` — team ID (use for allow_teams filtering; absent for personal/group chats)
- `serviceUrl` — base URL for outbound Bot Framework API calls (varies by region)

## Inbound Authentication: JWT Bearer Token

Teams sends: `Authorization: Bearer <jwt_token>`

The JWT is a standard 3-part base64url-encoded token (`header.payload.signature`).

### JWT Payload Claims to Verify

Decode the payload (middle part) with base64url decode → parse as JSON:

```json
{
  "aud": "<your_app_id>",
  "iss": "https://api.botframework.com",
  "iat": 1704067200,
  "exp": 1704070800,
  "nbf": 1704067200,
  "ver": "2.0"
}
```

Checks to perform (no RS256 signature verification — requires dynamic JWKS fetch):
- `aud` == your `app_id`
- `iss` is `"https://api.botframework.com"` OR `"https://sts.windows.net/{tenant_id}/"`
- `exp` > current time (not expired)
- `nbf` <= current time (not too early)

Note: Full RS256 signature verification would require fetching JWKS from `https://login.botframework.com/v1/.well-known/openidconfiguration` and verifying against the public key. This is not implemented (no RSA lib available) — claim-only validation is documented as a known limitation.

## Outbound Authentication: OAuth 2.0 Client Credentials

To send replies, first obtain an access token:

```
POST https://login.microsoftonline.com/{tenant_id}/oauth2/v2.0/token
Content-Type: application/x-www-form-urlencoded

grant_type=client_credentials
&client_id={app_id}
&client_secret={app_secret}
&scope=https%3A%2F%2Fapi.botframework.com%2F.default
```

Response:
```json
{
  "access_token": "eyJ...",
  "expires_in": 3599,
  "token_type": "Bearer"
}
```

Cache the token until `expires_in - 60` seconds have elapsed.

## Outbound: Sending a Reply

```
POST {serviceUrl}/v3/conversations/{conversation_id}/activities/{reply_to_id}
Authorization: Bearer {access_token}
Content-Type: application/json

{
  "type": "message",
  "text": "Hello!",
  "channelData": {
    "notification": {
      "alert": false
    }
  }
}
```

### Notification Suppression

Setting `channelData.notification.alert` to `false` prevents the message from generating a desktop/mobile notification (toast) in Teams. This is analogous to Telegram's `disable_notification: true`.

- **Final replies** and **ask_user_question** prompts use `alert: true` (notification).
- **Tool status** and other intermediate messages use `alert: false` (silent).

Or to start a new activity in the conversation (no reply_to_id):
```
POST {serviceUrl}/v3/conversations/{conversation_id}/activities
```

### @Mentions in Group Chats

In group chats (`conversation.isGroup == true`), replies include an `<at>` tag and `entities` array so the sender receives a notification:

```json
{
  "type": "message",
  "text": "<at>User Name</at> Hello!",
  "entities": [
    {
      "type": "mention",
      "mentioned": {
        "id": "<user_aad_object_id>",
        "name": "User Name"
      },
      "text": "<at>User Name</at>"
    }
  ]
}
```

The `mentioned.id` is the user's AAD object ID from `from.id` in the inbound activity. This is only added when `conversation.isGroup` is true and the sender's display name is available.

## Message Size Limits

- Maximum message length: **28,672 characters** (28 KB).
- Split long messages at a whitespace boundary and send as multiple activities.

## Editing an Activity

Update an existing activity (e.g. to edit a status message in-place):

```
PUT {serviceUrl}/v3/conversations/{conversation_id}/activities/{activity_id}
Authorization: Bearer {access_token}
Content-Type: application/json

{
  "type": "message",
  "textFormat": "markdown",
  "text": "Updated text"
}
```

Returns HTTP 200 on success.

## Deleting an Activity

Remove a previously sent activity:

```
DELETE {serviceUrl}/v3/conversations/{conversation_id}/activities/{activity_id}
Authorization: Bearer {access_token}
```

Returns HTTP 200 on success.

## Typing Indicator

Send a typing indicator to show the bot is working:

```
POST {serviceUrl}/v3/conversations/{conversation_id}/activities
Authorization: Bearer {access_token}
Content-Type: application/json

{
  "type": "typing"
}
```

## Sending Adaptive Cards

Send a rich card by including an `attachments` array in the activity:

```json
{
  "type": "message",
  "attachments": [
    {
      "contentType": "application/vnd.microsoft.card.adaptive",
      "content": {
        "$schema": "http://adaptivecards.io/schemas/adaptive-card.json",
        "type": "AdaptiveCard",
        "version": "1.4",
        "body": [ ... ]
      }
    }
  ]
}
```

## Uploading Attachments (Direct Line / Web Chat only)

**NOTE:** The Bot Framework attachment upload endpoint only works for Direct Line and
Web Chat channels. **Teams returns HTTP 404** for this endpoint. For Teams file
delivery, use the temp download server approach (see below).

Upload a file to a conversation via the Bot Framework attachment endpoint:

```
POST {serviceUrl}/v3/conversations/{conversation_id}/attachments
Authorization: Bearer {access_token}
Content-Type: application/json

{
  "type": "application/json",
  "name": "filename.json",
  "originalBase64": "<base64-encoded-content>"
}
```

Response:
```json
{
  "id": "<attachment_id>"
}
```

The uploaded attachment can be referenced via:
```
{serviceUrl}/v3/attachments/{attachment_id}/views/original
```

## Sending File Attachments (Direct Line / Web Chat only)

After uploading an attachment, send a message referencing it:

```json
{
  "type": "message",
  "text": "filename.json",
  "attachments": [
    {
      "contentType": "application/json",
      "contentUrl": "{serviceUrl}/v3/attachments/{attachment_id}/views/original",
      "name": "filename.json"
    }
  ]
}
```

## Teams File Delivery

Since Teams does not support the Bot Framework attachment upload API, file delivery
uses a layered approach controlled by `file_consent_cards` config (default: `true`).

### FileConsentCard Flow (Primary, `file_consent_cards: true`)

1. Bot sends a `FileConsentCard` attachment prompting user consent:
```json
{
  "type": "message",
  "attachments": [{
    "contentType": "application/vnd.microsoft.teams.card.file.consent",
    "name": "session_dump.json",
    "content": {
      "description": "Session debug dump",
      "sizeInBytes": 227899,
      "acceptContext": { "consentId": "<random-id>" },
      "declineContext": { "consentId": "<random-id>" }
    }
  }]
}
```
2. User clicks Accept → Teams sends an `invoke` activity with `name: "fileConsent/invoke"`.
3. Bot uploads file content to the `uploadUrl` from the invoke's `uploadInfo` via HTTP PUT.
4. Bot sends a `FileInfoCard` referencing the uploaded OneDrive file.
5. If user declines, bot acknowledges silently.

Pending consent data is stored in memory with a 10-minute TTL. Invoke activities are
handled synchronously (HTTP 200) unlike regular message activities (HTTP 202 + async).

### Temp Download URL (Fallback, or `file_consent_cards: false`)

1. Content is stored in `Temp_downloads` with a random token and 1-hour TTL.
2. The bot sends a Teams message containing a download URL: `{public_base_url}/downloads/{token}`.
3. The `public_base_url` is set from `tunnel.url` in config (required for this feature).
4. If no public URL is configured, the dump is sent as truncated text (25KB limit).
5. Expired entries are cleaned up by the daemon's periodic cleanup loop.

## Key Bot Framework Endpoints

| Endpoint | Purpose |
|----------|---------|
| `https://login.microsoftonline.com/{tenant}/oauth2/v2.0/token` | OAuth token fetch |
| `{serviceUrl}/v3/conversations/{conv_id}/activities` | Send activity (new) |
| `{serviceUrl}/v3/conversations/{conv_id}/activities/{activity_id}` | Reply / edit / delete activity |
| `{serviceUrl}/v3/conversations/{conv_id}/attachments` | Upload attachment |
| `{serviceUrl}/v3/attachments/{att_id}/views/original` | Retrieve uploaded attachment |

## Azure Setup Steps

1. Register an app in Azure Active Directory (App registrations → New registration).
2. Note the Application (client) ID → `app_id`.
3. Note the Directory (tenant) ID → `tenant_id`.
4. Create a client secret (Certificates & secrets) → `app_secret`.
5. Register a Bot in the Azure Bot Service (or Bot Framework developer portal):
   - Set Messaging Endpoint to `https://your-domain/teams/webhook` (or your configured path).
   - Set the Microsoft App ID to the AAD app ID above.
6. Add the bot to a Teams channel in the Bot Service resource.
7. In Teams: add the bot to a team or use it in a personal chat.

## Session Key Format

`teams:<team_id_or_personal>:<conversation_id>`

Sessions are shared per-channel: all users in the same Teams channel share one session. Sender identity is conveyed to the LLM via context headers (`sender=@<aad_id> (Display Name)`).

- For personal chats: team_id is `"personal"`. Since `conversation_id` is unique per 1:1 chat, this is effectively per-user.
- For group chats without a team: team_id is `"personal"` (when `channelData.team` is absent).
- For team channels: team_id is the channelData.team.id value.

## Reference

- Bot Framework Activity schema: https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-api-reference
- Bot Framework authentication: https://learn.microsoft.com/en-us/azure/bot-service/rest-api/bot-framework-rest-connector-authentication
- Teams bot setup: https://learn.microsoft.com/en-us/microsoftteams/platform/bots/how-to/create-a-bot-for-teams
