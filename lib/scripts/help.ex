defmodule Help do
  @moduledoc """
  Shows mambo help message.

  Examples:
    .help
  """

  use GenEvent.Behaviour

  @helpmsg """

  Mambo is an extensible irc-style teamspeak 3 bot.
  See '.help <option>' for more information on a specific script.

  Options:
  """

  @options [
    {Elixir.Admin, "admin"},
    {Elixir.Benis, "benis"},
    {Elixir.Brainfuck, "brainfuck"},
    {Elixir.Cannedreplies, "replies"},
    {Elixir.Eightball, "8ball"},
    {Elixir.Gif, "gif"},
    {Elixir.Google, "google"},
    {Elixir.Lastfm, "np"},
    {Elixir.Private, "private"},
    {Elixir.Quotes, "quotes"},
    {Elixir.Rainbow, "rainbow"},
    {Elixir.Random, "random"},
    {Elixir.Sux, "sux"},
    {Elixir.Title, "title"},
    {Elixir.Translate, "translate"},
    {Elixir.Twitter, "twitter"},
    {Elixir.Urban, "urban"},
    {Elixir.Utils, "utils"},
    {Elixir.Whatthecommit, "wtc"},
    {Elixir.Wolframalpha, "wolframalpha"},
    {Elixir.Youtube, "youtube"}
  ]

  def init(_) do
    {:ok, []}
  end

  def handle_event({:msg, {".help", _, {cid,_,_}}}, _) do
    options = Enum.map(Mambo.Bot.scripts(), &(@options[&1]))
      |> Enum.filter(&(&1 != nil))
    Mambo.Bot.send_msg("#{@helpmsg}#{Enum.join(options, " [b]|[/b] ")}", cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".help", _, {clid,_}}}, _) do
    options = Enum.map(Mambo.Bot.scripts(), &(@options[&1]))
      |> Enum.filter(&(&1 != nil))
    Mambo.Bot.send_privmsg("#{@helpmsg}#{Enum.join(options, " [b]|[/b] ")}", clid)
    {:ok, []}
  end

  def handle_event(_, _) do
    {:ok, []}
  end
end
