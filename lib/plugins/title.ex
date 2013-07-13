defmodule Title do
  use GenEvent.Behaviour

  @id elem(Tsmambo.Lib.consult("settings.cfg"), 1)[:bot_id]

  defp fetch(url, callback) do
    {:ok, ref} = :httpc.request(:get, {binary_to_list(url), []}, [],
                                sync: false, stream: :self, body_format: :binary)
    receive_chunk(ref, callback, <<>>, 5000)
  end

  defp receive_chunk(_ref, callback, body, len) when len <= 0 do
    [title] = Regex.run(%r/<title.*?>([\s\S]*?)<\/title>/, body, capture: [1])
    title = String.strip(title) |> String.strip(?\n)
    callback.("[b]Title:[/b] #{Tsmambo.Lib.decode_xml title}")
  end

  defp receive_chunk(ref, callback, body, len) do
    receive do
      {:http, {ref, :stream_start, headers}} ->
        content = list_to_binary(headers['content-type'])
        if String.contains?(content, "text/") do
          receive_chunk(ref, callback, body, len)
        else
          [ct | _rest] = String.split(content, ";")
          callback.("[b]Content Type:[/b] #{ct}")
        end

      {:http, {ref, :stream, data}} ->
        receive_chunk(ref, callback, body <> data, len - size(data))

      {:http, {ref, :stream_end, _headers}} ->
        receive_chunk(ref, callback, body, 0)
    after
      5000 ->
        :ok
    end
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({_, _, @id}, state) do
    {:ok, state}
  end

  def handle_event({msg, _user, _userid}, state) do
    case msg do
      # don't read omegle messages
      ["!o" | _] ->
        {:ok, state}
      _ ->
        msg = Enum.join(msg, " ")
        case Tsmambo.Lib.find_url(msg) do
          [url] ->
            callback = fn(x) -> :gen_server.cast(:mambo, {:send_txt, x}) end
            spawn(fn() -> fetch(url, callback) end)
            {:ok, state}
          _ ->
            {:ok, state}
        end
    end
  end

  def handle_event({:url, url}, state) do
    callback = fn(x) -> :gen_server.cast(:mambo, {:send_txt, x}) end
    spawn(fn -> fetch(url, callback) end)
    {:ok, state}
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
