defmodule Title do
  @moduledoc """
  Print the title of a webpage everytime its url is written on the chat.

  Examples:
    http://www.erlang.org/
    http://elixir-lang.org/
  """

  use GenEvent.Behaviour

  def init([]) do
    {:ok, []}
  end

  def handle_event({:msg, {".help title", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".help title", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
    {:ok, []}
  end

  # Ignore links from the `.gif` command.
  def handle_event({:msg, {<<".gif", _ :: binary>>, _, _}}, []) do
    {:ok, []}
  end

  def handle_event({:privmsg, {<<".gif", _ :: binary>>, _, _}}, []) do
    {:ok, []}
  end

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

  def handle_event(_, []) do
    {:ok, []}
  end

  @doc """
  If type is `:public` sends the title of the url `url` to the channel
  the bot is operating. Otherwise the title is sent to the private chat with
  id `type`.
  """
  def get_title(url, answer) do
    headers = [{"User-Agent", "Mozilla/5.0"},
               {"Cookie", "locale=en_US; path=/; domain=.facebook.com"}]
    case :hackney.get(url, headers, <<>>, [{:follow_redirect, true}]) do
      {:ok, 200, headers, client} ->
        case parse_content_type(headers["Content-Type"]) do
          "text/html" ->
            {:ok, body, _} = :hackney.body(5000, client)
            find_title(body, answer)
          {"text/html", :latin1} ->
            {:ok, body, _} = :hackney.body(5000, client)
            find_title(:unicode.characters_to_binary(body, :latin1), answer)
          other ->
            answer.("[b]Content Type:[/b] #{other}")
        end
      _ ->
        :ok
    end
  end

  # Helpers

  defp parse_content_type(content_type) do
    case String.split(content_type, ";", global: false) do
      ["text/html"] ->
        "text/html"
      [other] ->
        other
      ["text/html", rest] ->
        if String.contains?(rest, "8859-1") do
          {"text/html", :latin1}
        else
          "text/html"
        end
    end
  end

  defp find_title(body, answer) do
    case Regex.run(%r\<title[^>]*>([^<]+)</title>\im, body, capture: [1]) do
      [title] ->
        title = String.strip(title) |> String.split("\n") |> Enum.join
                |> Mambo.Helpers.decode_html
        answer.("[b]Title:[/b] #{title}")
      nil ->
        :ok
    end
  end
end
