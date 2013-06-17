defmodule Google do
  use GenEvent.Behaviour

  defp search(query, callback) do
    url = "https://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=#{query}"
    |> binary_to_list

    headers = [{'User-Agent', 'Mozilla/5.0 (mambo bot)'}]

    case :httpc.request(:get, {url, headers}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case get_url(body) do
          [url] ->
            callback.("Google: #{Tsmambo.Lib.format_url url}")
          nil ->
            callback.("Google: (no result)")
        end
      _ ->
        callback.("Well shit, something went wrong. I blame you.")
    end
  end

  defp get_url(body) do
    Regex.run(%r/unescapedUrl":"([\s\S]*?)","url/iu, body, capture: [1])
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({gen_server, msg, _user}, state) do
    case msg do
      ["!g", query] ->
        callback = fn(x) ->
                       :gen_server.cast(gen_server, {:send_txt, x})
                   end
        spawn(fn() -> search(URI.encode(query), callback) end)
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end
end
