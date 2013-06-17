defmodule Title do
  use GenEvent.Behaviour

  defp fetch(url, callback) do
    headers = [{'User-Agent', 'Mozilla/5.0 (mambo bot)'}]
    {:ok, ref} = :httpc.request(:get, {binary_to_list(url), headers}, [],
                                sync: false, stream: :self, body_format: :binary)
    receive_chunk(ref, callback, <<>>, 10000)
  end

  defp receive_chunk(_ref, callback, body, len) when len <= 0 do
    [title] = Regex.run(%r/<title.*?>([\s\S]*?)<\/title>/iu, body, capture: [1])
    callback.("Title: #{title}")
  end

  defp receive_chunk(ref, callback, body, len) do
    receive do
      {:http, {ref, :stream_start, headers}} ->
        content_type = ListDict.get(headers, 'content-type', '')
        case :lists.prefix('text/', content_type) do
          true ->
            receive_chunk(ref, callback, body, len)
          _ ->
            callback.("Content Type: #{content_type}")
        end

      {:http, {ref, :stream, data}} ->
        receive_chunk(ref, callback, body <> data, len - size(data))

      {:http, {ref, :stream_end, headers}} ->
        content_type = ListDict.get(headers, 'content-type')
        case :lists.prefix('text/', content_type) do
          true ->
            receive_chunk(ref, callback, body, 0)
          _ ->
            callback.("Content Type: #{content_type}")
        end
    after
      5000 ->
        :ok
    end
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({gen_server, msg, _user}, state) do
    msg = Enum.join(msg, " ")
    case Tsmambo.Lib.find_url(msg) do
      [url] ->
        callback = fn(x) ->
                     :gen_server.cast(gen_server, {:send_txt, x})
                   end
        spawn(fn() -> fetch(url, callback) end)
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end
end
