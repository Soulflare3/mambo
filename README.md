# Mambo

Mambo is an extensible teamspeak 3 chat bot. If you're using the bot, or have written
some scripts for it, please let me know, I'd love to hear what your doing if you don't
mind sharing it.

## Getting Started

You will need [elixir](http://elixir-lang.org/), see [this](http://elixir-lang.org/getting_started/1.html)
for installation instructions.

### Settings

Once elixir is installed you will have to create the `settings.json` file. This file contains all the
information needed for the bot to start. Here is an example:

```json
{
	"name": "mambo",
	"user": "server_query_username",
	"pass": "server_query_password",
	"host": "localhost",
	"port": 10011,
	"bot_id": "bot_teamspeak_unique_id_here",
	"admins": [],
	"scripts": [
		{"name": "Help", "args": []},
		{"name": "Utils", "args": []},
		{"name": "Title", "args": []},
		{"name": "Private", "args": []},
		{"name": "Translate", "args": []},
		{"name": "Calculator", "args": []},
	]
}
```

| Field     | Explanation                           |
|:---------:|:-------------------------------------:|
| `name`    | nickname that will appear in the chat |
| `user`    | server query username                 |
| `pass`    | server query password                 |
| `host`    | server ip address                     |
| `port`    | server query port                     |
| `bot_id`  | bot unique id                         |
| `admins`  | list of admins unique id              |
| `scripts` | list of scripts that the bot will use |

Use the `settings.json.sample` as a guide.

When you're happy with the `settings.json` file you can get the bot running with:

	mix deps.get --all
	mix compile
	iex -S mix

If you don't want an elixir shell and just want to run the bot in the background do
`elixir --detached --no-halt -S mix` instead of `iex -S mix`.

## Scripts

Mambo is extensible, it's easy to add new functionalities with scripting.

Scripts are written as gen_event handlers, see [1](http://www.erlang.org/doc/man/gen_event.html)
and [2](http://elixir-lang.org/docs/master/) for more info. Look at already written scripts to
know how to write your own, check this [list](https://github.com/mrshankly/mambo/tree/master/lib/scripts).

Once you have written your script place it in the `lib/scripts` folder and add it to the `scripts` list in
the `settings.json` file.

### Events

Scripts will receive notifications of the following events:

| Event                | Notification message                  |
|----------------------|---------------------------------------|
| chat message         | `{:msg, {msg, name, {cid, uid}}}`     |
| private chat message | `{:privmsg, {msg, name, {cid, uid}}}` |
| moved into channel   | `:move_in`                            |
| moved out of channel | `:move_out`                           |
| left the channel     | `:left`                               |
| entered the channel  | `{:enter, name}`                      |

### Reply

Scripts can reply back to the server using the following functions from the `Mambo.Bot` module:

#### Types

* `msg = String.t`
* `cid = integer`
* `reason = String.t`
* `time = integer`

#### Functions

* `send_msg(msg) :: :ok`
* `send_privmsg(msg, cid) :: :ok`
* `send_gm(msg) :: :ok`
* `kick(cid, reason) :: :ok`
* `ban(cid, time, reason) :: :ok`

### Storage

The module `Mambo.Brain` implements the methods to work with the bot memory, a key-value store.

## License

All files under this repository fall under the MIT License. Check
[LICENSE](https://github.com/mrshankly/mambo/blob/master/LICENSE) for more details.
