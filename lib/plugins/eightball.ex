defmodule Eightball do
  @options ["It is certain.", "It is decidedly so.", "Without a doubt.",
            "Yes, definitely.", "You may rely on it.", "As I see it, yes.",
            "Most likely.", "Outlook good.", "Yes.", "Signs point to yes.",
            "Reply hazy, try again.", "Ask again later.", "Better not tell you now.",
            "Cannot predict now.", "Concentrate and ask again.",
            "Don't count on it.", "My reply is no.", "My sources say no.",
            "Outlook not so good.", "Very doubtful."]

  defp shake() do
    Enum.at(@options, :random.uniform(Enum.count(@options)) - 1)
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, _userid}, state) do
    case msg do
      ["!8ball", _] ->
        :gen_server.cast(:mambo, {:send_txt, shake()})
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
