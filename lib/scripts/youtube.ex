defmodule Youtube do
  @moduledoc """
  Search youtube videos.

  Examples:
    .youtube <phrase>
  """

  use GenEvent.Behaviour

  def init(key) do
    {:ok, key}
  end

  def handle_event({:msg, {<<".help ", cmd :: binary>>, _, {cid,_,_}}}, key)
    when cmd in ["yt", "youtube"] do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
    {:ok, key}
  end

  def handle_event({:privmsg, {<<".help ", cmd :: binary>>, _, {clid,_}}}, key)
    when cmd in ["yt", "youtube"] do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
    {:ok, key}
  end

  def handle_event({:msg, {msg, _, {cid,_,_}}}, key) do
    answer = fn(x) -> Mambo.Bot.send_msg(x, cid) end
    spawn(fn -> parse_msg(msg, key, answer) end)
    {:ok, key}
  end

  def handle_event({:privmsg, {msg, _, {clid,_}}}, key) do
    answer = fn(x) -> Mambo.Bot.send_privmsg(x, clid) end
    spawn(fn -> parse_msg(msg, key, answer) end)
    {:ok, key}
  end

  def handle_event(_, key) do
    {:ok, key}
  end

  # Helpers

  defp parse_msg(msg, key, answer) do
    case Regex.run(%r/^(?:\.yt|\.youtube) (.*)/i, msg) do
      [_, query] ->
        youtube(query, key, answer)
      _ ->
        :ok
    end
  end

  defp youtube(query, key, answer) do
    url = "https://www.googleapis.com/youtube/v3/search?" <>
      URI.encode_query([q: query, key: key, part: "id"])

    case :hackney.get(url, [], <<>>, []) do
    {:ok, 200, _, client} ->
      {:ok, body, _} = :hackney.body(client)
      {:ok, data} = JSEX.decode(body)
      case data["items"] do
        [] ->
          answer.("No result.")
        videos ->
          case Enum.find(videos, fn(v) -> v["id"]["kind"] == "youtube#video" end) do
            nil ->
              answer.("No result.")
            video ->
              v_url = "https://www.youtube.com/watch?v=#{video["id"]["videoId"]}"
              spawn(Title, :get_title, [v_url, answer])
              answer.("#{Mambo.Helpers.format_url(v_url)}")
          end
      end
    _ ->
      answer.("Something went wrong.")
    end
  end
end
