defmodule Youtube do
  use GenEvent.Behaviour

  defp search(query, apikey, callback) do
    equery = URI.encode(query)
    url = "https://www.googleapis.com/youtube/v3/search?key=#{apikey}&part=id&q=#{equery}"
    case :httpc.request(:get, {binary_to_list(url), []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case :jsx.decode(body)["items"] do
          [] ->
            callback.("YouTube: (no result)")
          [first | _rest] ->
            video_url = "https://www.youtube.com/watch?v=#{first["id"]["videoId"]}"
            callback.("YouTube: #{Tsmambo.Lib.format_url video_url}")
        end
      _ ->
        callback.("Well shit, something went wrong. I blame you.")
    end
  end

  def init(apikey) do
    {:ok, apikey}
  end

  def handle_event({gen_server, msg, _user}, apikey) do
    case msg do
      ["!yt", query] ->
        callback = fn(x) ->
                       :gen_server.cast(gen_server, {:send_txt, x})
                   end
        spawn(fn() -> search(query, apikey, callback) end)
        {:ok, apikey}
      _ ->
        {:ok, apikey}
    end
  end
end
