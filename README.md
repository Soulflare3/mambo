# Mambo

Mambo is an extensible teamspeak 3 chat bot. If you're using the bot, or have written
some scripts for it, please let me know, I'd love to hear what your doing if you don't
mind sharing it.

## Getting Started

### Requirements
* Erlang (R16B or later) - there are pre built packages in the [Erlang Solutions Downloads page](https://www.erlang-solutions.com/downloads/download-erlang-otp)
* Elixir - see [here](http://elixir-lang.org/getting_started/1.html) for installation instructions
* Server query login credentials

Some plugins require access to certain APIs:
* [Imgur](http://api.imgur.com/) for `gif.ex`
* [Twitter](https://dev.twitter.com/) for `twitter.ex`
* [WolframAlpha](http://products.wolframalpha.com/api/) for `wolframalpha.ex`
* [YouTube](https://developers.google.com/youtube/) for `youtube.ex`
* [LastFM](http://www.last.fm/api) for `lastfm.ex`

Use the above links to register an account and get your API credentials. If you
don't plan to use the above plugins you can skip this.

* The script [`gif.ex`](https://github.com/mrshankly/mambo/blob/master/lib/scripts/gif.ex) also requires [ImageMagick](http://www.imagemagick.org/script/index.php) to be installed.

### Settings

Once you have sorted the dependencies you can start configuring the bot. Everything
related to configuration is done in the `settings.json` file.

| Field      | Explanation                                          |
|:----------:|:----------------------------------------------------:|
| `name`     | nickname that will appear in the chat                |
| `user`     | server query username                                |
| `pass`     | server query password                                |
| `host`     | server ip address                                    |
| `port`     | server query port                                    |
| `bot_id`   | bot unique id (Very important, the bot will not work |
|            | properly if this is not correct.)                    |
| `admins`   | list of admins unique id (Required if you plan to    |
|            | use the `admin.ex` script.)                          |
| `channels` | list of the channels the bot will join, this can be  |
|            | either a list of channel ids or the string "all" to  |
|            | join all the channels                                |
| `scripts`  | list of scripts that the bot will use (put only the  |
|            | scripts you want to use)                             |

You can use the file `settings.sample.json` as a guide and when you're happy
rename it to `settings.json`.

### Compile and run

When you have a working installation of elixir and the bot is properly configured,
download the [source code]() and extract it to a directory of your choosing.
Open a terminal window and do the following:

```sh
$ cd path/to/mambo
$ mix do deps.get, compile
```

If everything went ok you now have compiled the bot, next step is to actually
get it running, you have 2 choices:

To run the bot without a shell and in the background do:

```sh
$ elixir --detached --no-halt -S mix
```

To run the bot with an elixir shell do:

```sh
$ iex -S mix
```

I recommend running the bot with an elixir shell, this way you can have some
feedback and easily manage the bot without turning it off. See [Managing the bot]()
for more info. In linux (and Mac OS X too I guess) you can use `tmux` or `screen`
to keep the shell running. I don't know about Windows, google is your friend here.

## Scripts

Mambo by itself doesn't do much, but don't worry, it's extensible, you can add
new functionalities by writting your own scripts and/or using the provided scripts.

### Provided scripts

See the full list of the provided scripts [here](https://github.com/mrshankly/mambo/blob/master/lib/scripts).

Scripts are written as gen_event handlers, see [[1]](http://www.erlang.org/doc/man/gen_event.html)
and [[2]](http://elixir-lang.org/docs/master/) for more info. Look at already written scripts to
know how to write your own.

Once you have written your script place it in the `lib/scripts` folder and add it to the `scripts` list in
the `settings.json` file.

### Events

Scripts will receive notifications of the following events:

| Event                | Notification message                    |
|----------------------|-----------------------------------------|
| chat message         | `{:msg, {msg, name, {cid, clid, uid}}}` |
| private chat message | `{:privmsg, {msg, name, {clid, uid}}}`  |
| moved into channel   | `:move_in`                              |
| moved out of channel | `:move_out`                             |
| left the channel     | `:left`                                 |
| entered the channel  | `{:enter, name}`                        |

### Reply

Scripts can reply back to the server using the following functions from the `Mambo.Bot` module:

#### Types

* `msg = String.t`
* `cid = integer`
* `clid = integer`
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
