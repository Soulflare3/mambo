defmodule Images do
  use GenEvent.Behaviour

  defp image_me(s, callback) do
    es = URI.encode(s)
    url = "http://ajax.googleapis.com/ajax/services/search/images?safe=off&v=1.0&q=#{es}"
    case :httpc.request(:get, {binary_to_list(url), []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case :jsx.decode(body)["responseData"]["results"] do
          [] ->
            callback.("[b]Google Images:[/b] (no result)")
          [result | _] ->
            callback.("[b]Google Images:[/b] #{Tsmambo.Lib.format_url result["unescapedUrl"]}")
        end
      _ ->
        callback.("Well shit, something went wrong. I blame you.")
    end
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({gen_server, msg, _user, _userid}, state) do
    case msg do
      ["!images", s] ->
        callback = fn(x) ->
                        :gen_server.cast(gen_server, {:send_txt, x})
                   end
        spawn(fn() -> image_me(URI.encode(s), callback) end)
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end
end
