defmodule Google do
  use GenEvent.Behaviour

  defp search(query, scast, tnotify) do
    equery = URI.encode(query)
    url = "https://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=#{equery}"
    case :httpc.request(:get, {binary_to_list(url), []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case :jsx.decode(body)["responseData"]["results"] do
          [] ->
            scast.("[b]Google:[/b] (no result)")
          [result | _] ->
            scast.("[b]Google:[/b] #{Tsmambo.Lib.format_url result["unescapedUrl"]}")
            tnotify.(result["unescapedUrl"])
        end
      _ ->
        scast.("Well shit, something went wrong. I blame you.")
    end
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, _userid}, state) do
    case msg do
      ["!g", query] ->
        # send message to server
        scast = fn(x) -> :gen_server.cast(:mambo, {:send_txt, x}) end
        # notify plugins to get the url title
        tnotify = fn(l) -> Tsmambo.Plugins.notify({[l], "", ""}) end

        spawn(fn() -> search(query, scast, tnotify) end)
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end
end
