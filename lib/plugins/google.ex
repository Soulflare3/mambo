defmodule Google do
  use GenEvent.Behaviour

  defp search(query, callback) do
    equery = URI.encode(query)
    url = "https://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=#{equery}"
    case :httpc.request(:get, {binary_to_list(url), []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case :jsx.decode(body)["responseData"]["results"] do
          [] ->
            callback.("Google: (no result)")
          [result | _] ->
            callback.("Google: #{Tsmambo.Lib.format_url result["unescapedUrl"]}")
        end
      _ ->
        callback.("Well shit, something went wrong. I blame you.")
    end
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
        spawn(fn() -> search(query, callback) end)
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end
end
