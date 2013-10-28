defmodule Gif do
  @moduledoc """
  Resize a gif to the maximum allow size by teamspeak.

  Examples
    .gif <gif_link>
  """

  use GenEvent.Behaviour

  def init(clientID) do
    {:ok, clientID}
  end

  def handle_event({:msg, {".help gif", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".help gif", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
    {:ok, []}
  end

  def handle_event({:msg, {<<".gif ", url :: binary>>, name, {cid,_,_}}}, id) do
    answer = fn(x) -> Mambo.Bot.send_msg(x, cid) end
    spawn(fn -> get_gif(Mambo.Helpers.get_url(url), id, name, answer) end)
    {:ok, id}
  end

  def handle_event({:privmsg, {<<".gif ", url :: binary>>, name, {clid,_}}}, id) do
    answer = fn(x) -> Mambo.Bot.send_privmsg(x, clid) end
    spawn(fn -> get_gif(Mambo.Helpers.get_url(url), id, name, answer) end)
    {:ok, id}
  end

  def handle_event(_, id) do
    {:ok, id}
  end

  # Helpers.

  defp get_gif(url, id, name, answer) do
    case :hackney.get(url, [{"User-Agent", "Mozilla/5.0"}], <<>>, []) do
      {:ok, 200, headers, client} ->
        if headers["Content-Type"] == "image/gif" do
          answer.("This might take a while, hang in there.")
          {:ok, body, _} = :hackney.body(client)
          resize_gif(body, id, name, answer)
        else
          answer.("Hey smartass that's not a gif.")
        end
      _ ->
        answer.("Something went wrong.")
    end
  end

  defp resize_gif(data, id, name, answer) do
    original = "tmp/#{:erlang.phash2(make_ref())}.gif"
    small = "tmp/#{:erlang.phash2(make_ref())}.gif"
    unless File.exists?("tmp/") do
      File.mkdir!("tmp")
    end
    File.write!(original, data)
    case get_sizes(data) do
      {{300,h},_} when h <= 300 ->
        answer.("That gif already has the maximum allowed size.")
        File.rm(original)
      {{w,300},_} when w <= 300 ->
        answer.("That gif already has the maximum allowed size.")
        File.rm(original)
      {{w,h},{ws,hs}} ->
        System.cmd("convert -size #{w}x#{h} #{original} -resize #{ws}x#{hs} #{small}")
        if File.exists?(small) do
          upload_gif(small, id, name, answer)
          File.rm(original)
          File.rm(small)
        else
          answer.("Something went wrong.")
          File.rm(original)
        end
    end
  end

  defp get_sizes(gif) do
    <<?G, ?I, ?F, ?8, _, ?a,
      width :: [little, size(16)],
      height :: [little, size(16)],
      _rest :: binary>> = gif

    if width > height do
      width_s = 300
      height_s = trunc(height * (width_s / width))
    else
      height_s = 300
      width_s = trunc(width * (height_s / height))
    end

    {{width, height}, {width_s, height_s}}
  end

  def upload_gif(gif, clientID, name, answer) do
    case File.read(gif) do
      {:ok, bin} ->
        url = "https://api.imgur.com/3/upload"
        headers = [{"Authorization", "Client-ID #{clientID}"}]
        payload = {:form, [{"image", bin}]}
        case :hackney.post(url, headers, payload, []) do
          {:ok, 200, _, client} ->
            {:ok, body, _} = :hackney.body(client)
            new_gif = Mambo.Helpers.format_url(:jsx.decode(body)["data"]["link"])
            answer.("[b]#{name}[/b] here's your gif #{new_gif}")
          _ ->
            answer.("Something went wrong.")
        end
      _ ->
        answer.("Something went wrong.")
    end
  end
end
