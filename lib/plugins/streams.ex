defmodule Streams do
  use GenEvent.Behaviour

  @urls [{"!dota", 'https://api.twitch.tv/kraken/streams?game=Dota+2&limit='},
         {"!sc",   'https://api.twitch.tv/kraken/streams?game=StarCraft+II:+Heart+of+the+Swarm&limit='}]

  def get_list([], acc) do
      Enum.join(Enum.reverse(acc), " | ")
  end

  def get_list([h|t], acc) do
    url = Tsmambo.Lib.format_url(h["channel"]["url"], h["channel"]["display_name"])
    get_list(t, ["#{url} (#{h["viewers"]})" | acc])
  end

  def fetch(nil, _, _) do
    :ok
  end

  def fetch(url, count, callback) do
    case :httpc.request(:get, {url ++ count, []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        callback.("[b]Twitch:[/b] " <> get_list(:jsx.decode(body)["streams"], []))
      _ ->
        callback.("Well shit, something went wrong. I blame you.")
    end
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({gen_server, msg, _user, _userid}, state) do
    case msg do
      [cmd, count] ->
        case String.to_integer(count) do
          {num, ""} when num > 0 and num <= 10 ->
            callback = fn(x) ->
                         :gen_server.cast(gen_server, {:send_txt, x})
                       end
            spawn(fn() -> fetch(@urls[cmd], binary_to_list(count), callback) end)
            {:ok, state}
          _ ->
            {:ok, state}
        end

      [cmd] ->
        callback = fn(x) ->
                     :gen_server.cast(gen_server, {:send_txt, x})
                   end
        spawn(fn() -> fetch(@urls[cmd], '5', callback) end)
        {:ok, state}

      _ ->
        {:ok, state}
    end
  end
end
