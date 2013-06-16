defmodule Wolframalpha do
  use GenEvent.Behaviour

  defp search(query, apikey, callback) do
    url = "http://api.wolframalpha.com/v2/query?input=#{query}&appid=#{apikey}&podindex=2&format=plaintext"
    |> binary_to_list

    headers = [{'User-Agent', 'Mozilla/5.0 (mambo bot)'}]

    case :httpc.request(:get, {url, headers}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case get_value(body) do
          [value] ->
            callback.("Wolfram|Alpha: #{value}")
          nil ->
            callback.("Wolfram|Alpha: (no result)")
        end
      _ ->
        callback.("Well shit, something went wrong. I blame you.")
    end
  end

  defp get_value(body) do
    Regex.run(%r{<plaintext.*?>([\s\S]*?)</plaintext>}iu, body, capture: [1])
  end

  def init(apikey) do
    :inets.start()
    {:ok, apikey}
  end

  def handle_event({gen_server, msg, _user}, apikey) do
    case msg do
      ["!wa", query] ->
        callback = fn(x) ->
                       :gen_server.cast(gen_server, {:send_txt, x})
                   end
        spawn(fn() -> search(URI.encode(query), apikey, callback) end)
        {:ok, apikey}
      _ ->
        {:ok, apikey}
    end
  end
end
