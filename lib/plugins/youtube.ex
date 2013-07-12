defmodule Youtube do
  use GenEvent.Behaviour

  defp search(query, apikey, scast, tnotify) do
    equery = URI.encode(query)
    url = "https://www.googleapis.com/youtube/v3/search?key=#{apikey}&part=id&q=#{equery}"

    case :httpc.request(:get, {binary_to_list(url), []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case :jsx.decode(body)["items"] do
          [] ->
            scast.("[b]YouTube:[/b] (no result)")

          [first | _rest] ->
            video_url = "https://www.youtube.com/watch?v=#{first["id"]["videoId"]}"
            scast.("[b]YouTube:[/b] #{Tsmambo.Lib.format_url video_url}")
            tnotify.(video_url)
        end

      _ ->
        scast.("Well shit, something went wrong. I blame you.")
    end
  end

  def init(apikey) do
    {:ok, apikey}
  end

  def handle_event({msg, _user, _userid}, apikey) do
    case msg do
      ["!yt", query] ->
        # send message to server
        scast = fn(x) -> :gen_server.cast(:mambo, {:send_txt, x}) end
        # notify plugins to get the url title
        tnotify = fn(url) -> Tsmambo.Plugins.notify({:url, url}) end

        spawn(fn() -> search(query, apikey, scast, tnotify) end)
        {:ok, apikey}

      _ ->
        {:ok, apikey}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
