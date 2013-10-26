defmodule Help do
  @moduledoc """
  Shows mambo help message.

  Examples:
    .help
  """

  use GenEvent.Behaviour

  @helpmsg """

  Mambo is an extensible irc-style teamspeak 3 bot.
  See 'help <option>' for more information on a specific script.

  Options:
    ask                 - ask mambo anything
    benis               - benisify a sentence (*)
    bf                  - brainfuck interpreter
    reply | replies     - reply to certain keywords (*)
    chat | speak | talk - start a private chat with the bot (*)
    8ball               - ask the magic 8 ball
    gay                 - rainbow color a phrase (*)
    np                  - shows the last (or current) played song in last.fm (*)
    calc                - calculator
    random              - luck games
    google | yt | img   - search google, youtube and images
    sux                 - curse something (*)
    title               - print url title (*)
    tl | translate      - translate an expression
    utils               - utility commands
    wtc                 - prints a commit from whatthecommit.com

    Options marked with (*) don't work on a private chat.
  """

  def init([]) do
    {:ok, []}
  end

  def handle_event({:msg, {".help", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(@helpmsg, cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".help", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(@helpmsg, clid)
    {:ok, []}
  end

  def handle_event(_, []) do
    {:ok, []}
  end
end
