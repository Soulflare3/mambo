defmodule Images do
  use GenEvent.Behaviour

  def image_me(s, scast, tnotify) do
    es = URI.encode(s)
    url = "http://ajax.googleapis.com/ajax/services/search/images?safe=off&v=1.0&q=#{es}"
    case :httpc.request(:get, {String.to_char_list!(url), []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case :jsx.decode(body)["responseData"]["results"] do
          [] ->
            scast.("[b]Google Images:[/b] (no result)")
          [result | _] ->
            scast.("[b]Google Images:[/b] #{Tsmambo.Lib.format_url result["unescapedUrl"]}")
            tnotify.(result["unescapedUrl"])
        end
      _ ->
        scast.("Well shit, something went wrong. I blame you.")
    end
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, _userid, :unmuted}, state) do
    case msg do
      ["!images", s] ->
        # send message to server
        scast = fn(x) -> :gen_server.cast(:mambo, {:send_txt, x}) end
        # notify plugins to get the url title
        tnotify = fn(url) -> Tsmambo.Plugins.notify({:url, url}) end

        spawn(Images, :image_me, [URI.encode(s), scast, tnotify])
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
