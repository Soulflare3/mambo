defmodule Question do
  @moduledoc """
  Ask mambo anything.

  Examples:
    mambo where is <place>
    mambo what does <expression> mean
  """

  use GenEvent.Behaviour

  @name "My name is Wolfram|Alpha."
  @author "I was created by Stephen Wolfram and his team."

  @doc false
  def init(apikey) do
    {:ok, re} = Regex.compile("^#{Mambo.Bot.name} ((what|who|where|why|when|" <>
      "who|whom|how|whose|whence|whither|do)('s)? (.*))", "i")
    {:ok, {re, apikey}}
  end

  @doc false
  def handle_event({:msg, {".help ask", _, _}}, state) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
    {:ok, state}
  end

  @doc false
  def handle_event({:privmsg, {".help ask", _, {id, _}}}, state) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
    {:ok, state}
  end

  @doc false
  def handle_event({:msg, {msg, _, _}}, {re, key}) do
    answer = fn(x) -> Mambo.Bot.send_msg(x) end

    case Regex.run(re, msg, capture: [1]) do
      [exp] ->
        spawn(fn -> ask(exp, answer, key) end)
        {:ok, {re, key}}
      _ ->
        {:ok, {re, key}}
    end
  end

  @doc false
  def handle_event({:privmsg, {msg, _, {id, _}}}, {re, key}) do
    answer = fn(x) -> Mambo.Bot.send_privmsg(x, id) end

    case Regex.run(re, msg, capture: [1]) do
      [exp] ->
        spawn(fn -> ask(exp, answer, key) end)
        {:ok, {re, key}}
      _ ->
        {:ok, {re, key}}
    end
  end

  @doc false
  def handle_event(_, state) do
    {:ok, state}
  end

  # --------
  # Helpers
  # --------

  defp ask(q, answer, key) do
    url = "http://api.wolframalpha.com/v2/query?" <>
      URI.encode_query(
        [input: q,
         appid: key,
         podindex: 2,
         format: "plaintext"]) |> String.to_char_list!

    case :httpc.request(:get, {url, []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case parse_resp(body) do
          {:done, _, s, _, _} ->
            answer.(format(s))
          {:no_result, _, s, _, _} ->
            answer.(s)
        end
      _ ->
        answer.("Something went wrong.")
    end
  end

  defp parse_resp(xml) do
    :xmerl_sax_parser.stream(xml, event_fun: &event/3, event_state: "")
  end

  defp event({:startElement, _, 'queryresult', _, [h | _]}, _, state) do
    if elem(h, 3) == 'true' do
      state
    else
      throw {:no_result, "Sorry, I'm afraid I can't help you with that."}
    end
  end

  defp event({:characters, chars}, _, _) do
    chars
  end

  defp event({:endElement, _, 'plaintext', _}, _, state) do
    throw {:done, String.from_char_list!(state)}
  end

  defp event(_, _, state) do
    state
  end

  defp format(s) do
    String.replace(s, " \n ", " ")
    |> String.replace(" | ", ": ")
    |> String.replace("  ", " ")
    |> String.split("\n")
    |> Enum.join(" | ")
    |> String.replace(@name, "My name is #{Mambo.Bot.name}.")
    |> String.replace(@author, "I was created by MrShankly.")
  end
end
