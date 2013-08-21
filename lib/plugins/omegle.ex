defmodule Omegle do
  use GenEvent.Behaviour

  @url 'http://front4.omegle.com'

  @headers [{'User-Agent', 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:22.0) Gecko/20100101 Firefox/22.0'},
            {'Connection', 'keep-alive'},
            {'Accept', 'application/json'},
            {'Accept-Language', 'en-us,en;q=0.5'},
            {'Content-Type', 'application/x-www-form-urlencoded; charset=utf-8'},
            {'Referer', 'http://omegle.com/'}]

  defp connect do
    case post('/start', 'rcs=1&spid=') do
      {:ok, id} -> Enum.filter id, &1 != ?"
      :error    -> :error
    end
  end

  def send(id, msg, callback) do
    case post('/send','msg=' ++ msg ++ '&id=' ++ id) do
      {:ok, 'win'} ->
        :ok
      _ ->
        callback.("[b]Omegle:[/b] Failed to send the message.")
    end
  end

  def events(_, callback, 5) do
    callback.("[b]Omegle:[/b] Timeout.")
    :timeout
  end

  def events(id, callback, count) do
    case post('/events', 'id=' ++ id) do
      {:ok, body} ->
        :timer.sleep 2000

        info = :jsx.decode(list_to_binary body)

        if ["connected"] in info do
            callback.("[b]Omegle:[/b] Wild Stranger appeared!")
        end

        case check_messages(info) do
          {:msg, msg} ->
            callback.("[b]Stranger:[/b] #{msg}")
            events(id, callback, 0)
          :disconnect ->
            callback.("[b]Omegle:[/b] The wild Stranger fled!")
            Tsmambo.Plugins.notify(:reset)
            :ok
          :nothing ->
            events(id, callback, 0)
        end
      :error ->
        events(id, callback, count + 1)
    end
  end

  defp check_messages([["typing"], ["gotMessage", msg] | _t]), do: {:msg, msg}
  defp check_messages([["gotMessage", msg] | _t]),             do: {:msg, msg}
  defp check_messages([["typing"] | _t]),                      do: :nothing
  defp check_messages([["waiting"] | _t]),                     do: :nothing
  defp check_messages([["strangerDisconnected"] | _t]),        do: :disconnect
  defp check_messages(other) do
    if ["strangerDisconnected"] in other do
      :disconnect
    else
      :nothing
    end
  end

  def post(where, params // []) do
    case :httpc.request(:post, {@url ++ where,
                                @headers,
                                'application/x-www-form-urlencoded', params}, [], []) do
      {:ok, {{_, 200, _}, _, body}} ->
        {:ok, body}
      _ ->
        :error
    end
  end

  def init(_args) do
    {:ok, {[], nil}}
  end

  def handle_event({msg, _user, _userid, :unmuted}, {[], nil} = state) do
    callback = fn(x) ->
                 :gen_server.cast(:mambo, {:send_txt, x})
               end

    case msg do
      ["!o"] ->
        case connect do
          :error ->
            callback.("[b]Omegle:[/b] Unable to connect!")
            {:ok, state}
          id ->
            callback.("[b]Omegle:[/b] Waiting for someone...")
            pid = spawn(Omegle, :events, [id, callback, 0])
            {:ok, {id, pid}}
        end
      ["!o", _msg] ->
        callback.("[b]Omegle:[/b] No active conversation going on.")
        {:ok, state}
      ["!dc"] ->
        callback.("[b]Omegle:[/b] No active conversation going on.")
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end

  def handle_event({msg, _user, _userid, :unmuted}, {id, pid} = state) do
    callback = fn(x) ->
                 :gen_server.cast(:mambo, {:send_txt, x})
               end

    case msg do
      ["!o"] ->
        callback.("[b]Omegle:[/b] Disconnect from the current conversation first.")
        {:ok, state}
      ["!o", msg] ->
        spawn(Omegle, :send, [id, String.to_char_list!(URI.encode msg), callback])
        {:ok, state}
      ["!dc"] ->
        spawn(Omegle, :post, ['/disconnect'])
        Process.exit(pid, :kill)
        callback.("[b]Omegle:[/b] Got away safely!")
        {:ok, {[], nil}}
      _ ->
        {:ok, state}
    end
  end

  def handle_event(:reset, _state) do
    {:ok, {[], nil}}
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
