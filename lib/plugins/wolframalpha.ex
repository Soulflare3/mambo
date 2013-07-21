defmodule Wolframalpha do
  use GenEvent.Behaviour

  defp search(query, apikey, callback) do
    equery = URI.encode(query)
    url = "http://api.wolframalpha.com/v2/query?input=#{equery}&appid=#{apikey}&podindex=2&format=plaintext"
    case :httpc.request(:get, {binary_to_list(url), []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case get_value(body) do
          [value] ->
            callback.("[b]Wolfram|Alpha:[/b] #{Tsmambo.Lib.decode_xml value}")
          nil ->
            callback.("[b]Wolfram|Alpha:[/b] (no result)")
        end
      _ ->
        callback.("Well shit, something went wrong. I blame you.")
    end
  end

  defp get_value(body) do
    Regex.run(%r/<plaintext.*?>([\s\S]*?)<\/plaintext>/iu, body, capture: [1])
  end

  def init(apikey) do
    {:ok, apikey}
  end

  def handle_event({msg, _user, _userid, :unmuted}, apikey) do
    case msg do
      ["!wa", query] ->
        callback = fn(x) ->
                       :gen_server.cast(:mambo, {:send_txt, x})
                   end
        spawn(fn() -> search(query, apikey, callback) end)
        {:ok, apikey}
      _ ->
        {:ok, apikey}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
