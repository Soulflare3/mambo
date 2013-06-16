defmodule Tsmambo.Client do
  use GenServer.Behaviour

  def start_link(settings) do
    login = {settings[:address],
             settings[:port],
             settings[:username],
             settings[:password]}
    :gen_server.start_link({:local, :mambo}, __MODULE__,
                           {login, settings[:name], settings[:plugins]}, [])
  end

  defp send_msg(socket, msg) do
    :gen_tcp.send(socket, "#{msg}\n")
  end

  defp login(socket, user, pass, name) do
    :ok = send_msg(socket, "login #{user} #{pass}")
    :ok = send_msg(socket, "use sid=1")
    :ok = send_msg(socket, "servernotifyregister event=textchannel id=1")
    send_msg(socket, "clientupdate client_nickname=#{name}")
  end

  defp handle_msg(pid, "notifytextmessage" <> data) do
    msg = Regex.run(%r{(?<=msg=)(.*?)(?= invokerid=)}iu, data, capture: :first)
    |> hd
    |> Tsmambo.Lib.decode

    user = Regex.run(%r{(?<=invokername=)(.*?)(?= invokeruid=)}iu, data, capture: :first)
    |> hd
    |> Tsmambo.Lib.decode

    Tsmambo.Plugins.notify(pid, {self(), Regex.split(%r{ }, msg, parts: 2), user})
  end

  defp handle_msg(_pid, _data) do
    :ok
  end

  def init({{address, port, user, pass}, name, plugins}) do
    case :gen_tcp.connect(address, port, [:binary]) do
      {:ok, socket} ->
        IO.puts("Connected to #{address}:#{port}")
        {:ok, pid} = Tsmambo.Plugins.start_link(plugins)
        :ok = login(socket, user, pass, name)
        :erlang.send_after(300000, self(), :ping)
        {:ok, {socket, pid}}
      {:error, reason} ->
        {:stop, reason}
    end
  end

  def handle_cast({:send_txt, msg}, {socket, _pid} = state) do
    :ok = send_msg(socket, "sendtextmessage targetmode=2 target=1 msg=#{Tsmambo.Lib.encode(msg)}")
    {:noreply, state}
  end

  def handle_info({:tcp, _socket, data}, {_socket, pid} = state) do
    handle_msg(pid, data)
    {:noreply, state}
  end

  def handle_info({:tcp_close, _socket}, state) do
    {:stop, :normal, state}
  end

  def handle_info(:ping, {socket, _pid} = state) do
    :ok = send_msg(socket, "version")
    :erlang.send_after(300000, self(), :ping)
    {:noreply, state}
  end

  def handle_info(_info, state) do
    {:noreply, state}
  end
end
