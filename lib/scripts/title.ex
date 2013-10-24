defmodule Title do
  @moduledoc """
  Print the title of a webpage everytime its url is written on the chat.

  Examples:
    http://www.erlang.org/
    http://elixir-lang.org/
  """

  use GenEvent.Behaviour

  @doc false
  def init([]) do
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {".help title", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
    {:ok, []}
  end

  @doc false
  def handle_event({:privmsg, {".help title", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {msg, _, {cid,_,_}}}, []) do
    answer = fn(x) -> Mambo.Bot.send_msg(x, cid) end
    case Mambo.Helpers.find_url(msg) do
      nil ->
        {:ok, []}
      url ->
        spawn(fn -> get_title(url, answer) end)
        {:ok, []}
    end
  end

  @doc false
  def handle_event(_, []) do
    {:ok, []}
  end

  @doc """
  If type is `:public` sends the title of the url `url` to the channel
  the bot is operating. Otherwise the title is sent to the private chat with
  id `type`.
  """
  def get_title(url, answer) do
    headers = [{'User-Agent', 'Mozilla/5.0'},
               {'Cookie', 'locale=en_US; path=/; domain=.facebook.com'}]

    {:ok, ref} = :httpc.request(:get, {String.to_char_list!(url), headers}, [],
      sync: false, stream: :self)

    receive_chunk(ref, <<>>, 5000, answer)
  end

  # --------
  # Helpers
  # --------

  defp receive_chunk(_, body, len, answer) when len <= 0 do
    body = :unicode.characters_to_binary(body, :latin1)
    case Regex.run(%r\<title[^>]*>([^<]+)</title>\im, body, capture: [1]) do
      nil ->
        :ok
      [title] ->
        title = String.strip(title) |> String.split("\n") |> Enum.join
                |> Mambo.Helpers.decode_html

        answer.("[b]Title:[/b] #{title}")
    end
  end

  defp receive_chunk(ref, body, len, answer) do
    receive do
      {:http, {^ref, :stream_start, headers}} ->
        [ct|_] = String.from_char_list!(headers['content-type'])
          |> String.split(";", global: false)

        if ct == "text/html" do
          receive_chunk(ref, body, len, answer)
        else
          answer.("[b]Content Type:[/b] #{ct}")
        end

      {:http, {^ref, :stream, data}} ->
        receive_chunk(ref, body <> data, len - size(data), answer)

      {:http, {^ref, :stream_end, _}} ->
        receive_chunk(ref, body, 0, answer)

    after
      5000 ->
        :ok
    end
  end
end
