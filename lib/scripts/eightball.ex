defmodule Eightball do
  @moduledoc """
  Ask the magic 8 ball.

  Examples:
    .8ball <question>
  """

  use GenEvent.Behaviour

   @options ["It is certain.", "It is decidedly so.", "Without a doubt.",
             "Yes, definitely.", "You may rely on it.", "As I see it, yes.",
             "Most likely.", "Outlook good.", "Yes.", "Signs point to yes.",
             "Reply hazy, try again.", "Ask again later.", "Better not tell you now.",
             "Cannot predict now.", "Concentrate and ask again.",
             "Don't count on it.", "My reply is no.", "My sources say no.",
             "Outlook not so good.", "Very doubtful."]

  @doc false
  def init([]) do
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {".help 8ball", _, _}}, []) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
    {:ok, []}
  end

  @doc false
  def handle_event({:privmsg, {".help 8ball", _, {id, _}}}, []) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {<<".8ball ", _ :: binary>>, _, _}}, []) do
    Mambo.Bot.send_msg(shake)
    {:ok, []}
  end

  @doc false
  def handle_event({:privmsg, {<<".8ball ", _ :: binary>>, _, {id, _}}}, []) do
    Mambo.Bot.send_privmsg(shake, id)
    {:ok, []}
  end

  @doc false
  def handle_event(_, []) do
    {:ok, []}
  end

  # --------
  # Helpers
  # --------

  defp shake() do
    Enum.at(@options, :random.uniform(Enum.count(@options)) - 1)
  end
end
