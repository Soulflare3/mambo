defmodule Mambo.Watcher do
  @moduledoc """
  Responsible for watching the chat of a channel and notifying the scripts when
  a chat message comes up.
  """

  use GenServer.Behaviour

  @login_ok     "error id=0 msg=ok\n\r"
  @notify_msg   "notifytextmessage"
  @notify_move  "notifyclientmoved"
  @notify_left  "notifyclientleftview"
  @notify_enter "notifycliententerview"

  # API.

  @spec start_link(any()) :: {:ok, pid}
  def start_link(args) do
    {:ok, _} = :gen_server.start_link(__MODULE__, args, [])
  end

  # Helpers.

  defp send_to_server(socket, msg) do
    :ok = :gen_tcp.send(socket, "#{msg}\n")
  end

  defp login(socket, id, name, user, pass) do
    [ "login #{user} #{pass}", "use sid=1",
      "servernotifyregister event=channel id=#{id}",
      "servernotifyregister event=textchannel id=#{id}",
      "clientupdate client_nickname=#{Mambo.Helpers.escape(name)}", "whoami" ]
    |> Enum.each(fn(x) -> send_to_server(socket, x) end)
  end

  # gen_server callbacks

  def init({cid, bot_id, {name, host, port, user, pass}}) do
    {:ok, socket} = :gen_tcp.connect(String.to_char_list!(host), port, [:binary])
    login(socket, cid, name, user, pass)
    :erlang.send_after(300000, self(), :keep_alive)
    {:ok, {socket, cid, bot_id}}
  end

  def handle_cast({:send_msg, msg}, {socket, _} = state) do
    cmd = "sendtextmessage targetmode=2 target=1 msg=#{Mambo.Helpers.escape(msg)}"
    send_to_server(socket, cmd)
    {:noreply, state}
  end

  def handle_cast(_, state) do
    {:noreplay, state}
  end

  # If login went ok, move watcher to channel with id `cid`.
  def handle_info({:tcp, _, <<@login_ok, r :: binary>>}, {socket, cid, bot_id} = state) do
    case Regex.run(%r/client_id=(\d*)/, r) do
      [_, clid] ->
        send_to_server(socket, "clientmove clid=#{clid} cid=#{cid}")
        {:noreply, {socket, {clid, cid, bot_id}}}
      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:tcp, _, <<@notify_msg, r :: binary>>}, {_, {_,cid,bid}} = state) do
    {:ok, re} = Regex.compile("targetmode=([1-2]) msg=(\\S*)(?: target=\\d*)? " <>
      "invokerid=(\\d*) invokername=(.*) invokeruid=(.*)", "i")

    case Regex.run(re, r) do
      [_, _, _, _, _, ^bid] ->
        {:noreply, state}

      [_, "2", msg, clid, name, uid] ->
        msg = Mambo.Helpers.unescape(msg)
        name = Mambo.Helpers.unescape(name)
        Mambo.EventManager.notify({:msg, {msg, name, {cid, clid, uid}}})
        {:noreply, state}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:tcp, _, <<@notify_move, r :: binary>>}, {_, {_,cid,_}} = state) do
    cid = integer_to_binary(cid)
    case Regex.run(%r/ctid=([0-9]*)/i, r) do
      [_, ^cid] ->
        Mambo.EventManager.notify({:move_in, cid})
        {:noreply, state}

      [_, lcid] ->
        Mambo.EventManager.notify({:move_out, lcid})
        {:noreply, state}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:tcp, _, <<@notify_left, _ :: binary>>}, state) do
    Mambo.EventManager.notify(:left)
    {:noreply, state}
  end

  def handle_info({:tcp, _, <<@notify_enter, r :: binary>>}, state) do
    case Regex.run(%r/client_nickname=(.*) client_input_muted=/i, r) do
      [_, name] ->
        Mambo.EventManager.notify({:enter, name})
        {:noreply, state}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:tcp_closed, _}, state) do
    {:stop, :normal, state}
  end

  def handle_info(:keep_alive, {s, _} = state) do
    send_to_server(s, "version")
    :erlang.send_after(300000, self(), :keep_alive)
    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end
end
