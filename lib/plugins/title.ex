defmodule Title do
  use GenEvent.Behaviour

  @id elem(Tsmambo.Lib.consult("settings.cfg"), 1)[:bot_id]

  def fetch(url, callback) do
    case System.cmd(<<"bin/title" :: binary, ? , ?", url :: binary, ?") do
      <<?t, title :: binary>> ->
        callback.("[b]Title:[/b] #{Tsmambo.Lib.decode_html title}")
      <<?c, content :: binary>> ->
        callback.("[b]Content Type:[/b] #{content}")
      _ ->
        :ok
    end
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({_, _, @id, _}, state) do
    {:ok, state}
  end

  def handle_event({msg, _user, _userid, :unmuted}, state) do
    case msg do
      # don't read omegle messages
      ["!o" | _] ->
        {:ok, state}
      _ ->
        msg = Enum.join(msg, " ")
        case Tsmambo.Lib.find_url(msg) do
          [url] ->
            callback = fn(x) -> :gen_server.cast(:mambo, {:send_txt, x}) end
            spawn(Title, :fetch, [url, callback])
            {:ok, state}
          _ ->
            {:ok, state}
        end
    end
  end

  def handle_event({:url, url}, state) do
    callback = fn(x) -> :gen_server.cast(:mambo, {:send_txt, x}) end
    spawn(Title, :fetch, [url, callback])
    {:ok, state}
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
