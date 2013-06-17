defmodule Tsmambo.Client do
  use GenServer.Behaviour

  @msg_template "sendtextmessage targetmode=2 target=1 msg="

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
    send_msg(socket, "clientupdate client_nickname=#{Tsmambo.Lib.encode name}")
  end

  def plugin_exists?(plugin, list) do
    Enum.find(list, fn({_, {x, _}}) -> plugin == x end)
  end

  defp handle_msg(pid, "notifytextmessage" <> data) do
    [msg] = Regex.run(%r/msg=([\s\S]*?) invokerid=/, data, capture: [1])
    msg = Tsmambo.Lib.decode(msg)

    [user] = Regex.run(%r/invokername=([\s\S]*?) invokeruid=/, data, capture: [1])
    user = Tsmambo.Lib.decode(user)

    [userid] = Regex.run(%r/invokeruid=.*/, data)
    userid = String.replace(userid, "invokeruid=", "")

    Tsmambo.Plugins.notify(pid, {self(), String.split(msg, " ", global: false), user, userid})
  end

  defp handle_msg(_pid, _data) do
    :ok
  end

  ### :gen_server callbacks

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

  ## Send text message
  def handle_cast({:send_txt, msg}, {socket, _pid} = state) do
    :ok = send_msg(socket, "#{@msg_template}#{Tsmambo.Lib.encode msg}")
    {:noreply, state}
  end

  ## Plugin management
  def handle_cast({:load_plugin, plugin}, {socket, pid} = state) do
    if plugin in Tsmambo.Plugins.which_handlers(pid) do
      :ok = send_msg(socket, "#{@msg_template}#{Tsmambo.Lib.encode "[b]#{plugin}[/b] already loaded."}")
      {:noreply, state}
    else
      {:ok, settings} = Tsmambo.Lib.consult("settings.cfg")
      case plugin_exists?(plugin, settings[:plugins]) do
        nil ->
          :ok = send_msg(socket, "#{@msg_template}#{Tsmambo.Lib.encode "[b]#{plugin}[/b] not found."}")
          {:noreply, state}

        {_, {^plugin, args}} ->
          case Tsmambo.Plugins.add_handler(pid, plugin, args) do
            :ok ->
              info = Tsmambo.Lib.encode "[b]#{plugin}[/b] loaded."
              :ok = send_msg(socket, "#{@msg_template}#{info}")
              {:noreply, state}

            {:error, reason} ->
              :ok = send_msg(socket, "#{@msg_template}#{Tsmambo.Lib.encode reason}")
              {:noreply, state}
          end
      end
    end
  end

  def handle_cast({:unload_plugin, plugin}, {socket, pid} = state) do
    if plugin in Tsmambo.Plugins.which_handlers(pid) do
      case Tsmambo.Plugins.delete_handler(pid, plugin, []) do
        :ok ->
          info = Tsmambo.Lib.encode "[b]#{plugin}[/b] unloaded."
          :ok = send_msg(socket, "#{@msg_template}#{info}")
          {:noreply, state}

        {:error, reason} ->
          :ok = send_msg(socket, "#{@msg_template}#{Tsmambo.Lib.encode reason}")
          {:noreply, state}
      end
    else
      :ok = send_msg(socket, "#{@msg_template}#{Tsmambo.Lib.encode "[b]#{plugin}[/b] already unloaded."}")
      {:noreply, state}
    end
  end

  def handle_cast(:list_plugins, {socket, pid} = state) do
    f = fn(x) -> String.replace(atom_to_binary(x), "Elixir.", "") end

    # Ignore the head, since it's Plugin.Manager
    [_ | lp] = Enum.reduce(Tsmambo.Plugins.which_handlers(pid), [], fn(x, acc) -> [f.(x) | acc] end)
    sp = Enum.join(lp, " | ")

    send_msg(socket, "#{@msg_template}#{Tsmambo.Lib.encode sp}")
    {:noreply, state}
  end

  ## Server messages
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
