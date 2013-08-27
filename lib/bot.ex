defmodule Mambo.Bot do
	@moduledoc """
	Responsible for connecting to the teamspeak server, also acts as a bridge
	between the teamspeak server and the plugins. Supervised by `Mambo.Supervisor`.
	"""

	use GenServer.Behaviour

	@bot __MODULE__

	@notify_msg       "notifytextmessage"
	@notify_move      "notifyclientmoved"
	@notify_left      "notifyclientleftview"
	@notify_enter     "notifycliententerview"

	defrecord Settings,
		name:    "mambo",
		user:    "username",
		pass:    "password",
		host:    "localhost",
		port:    10011,
		bot_id:  "",
		admins:  [],
		scripts: []

	@doc """
	Starts bot process. Returns `{:ok, pid}` on success.
	"""
	@spec start_link() :: {:ok, pid}
	def start_link() do
		s = Mambo.Helpers.get_settings
		{:ok, _} = :gen_server.start_link({:local, @bot}, __MODULE__, s, [])
	end

	@doc """
	Sends a chat message to the teamspeak server.
	"""
	@spec send_msg(String.t) :: :ok
	def send_msg(msg) do
		:ok = :gen_server.cast(@bot, {:send_msg, msg})
	end

	@doc """
	Sends a private message to the teamspeak client with the id `cid`.
	"""
	@spec send_privmsg(String.t, integer) :: :ok
	def send_privmsg(msg, cid) do
		:ok = :gen_server.cast(@bot, {:send_privmsg, {msg, cid}})
	end

	@doc """
	Sends a global chat message to the teamspeak server.
	"""
	@spec send_gm(String.t) :: :ok
	def send_gm(msg) do
		:ok = :gen_server.cast(@bot, {:send_gm, msg})
	end

	@doc """
	Kick a client from the teamspeak server.
	"""
	@spec kick(integer, String.t) :: :ok
	def kick(cid, msg // "Smells bad!") do
		:ok = :gen_server.cast(@bot, {:kick, {cid, msg}})
	end

	@doc """
	Bans a client from the teamspeak server.
	"""
	@spec ban(integer, integer, String.t) :: :ok
	def ban(cid, time, msg // "Smells bad!") do
		:ok = :gen_server.cast(@bot, {:ban, {cid, time, msg}})
	end

	@doc """
	Returns the bot name.
	"""
	@spec name() :: String.t
	def name() do
		:gen_server.call(@bot, :name)
	end

	# --------
	# Helpers
	# --------

	defp send_to_server(socket, msg) do
		:ok = :gen_tcp.send(socket, "#{msg}\n")
	end

	defp login(s, name, user, pass) do
		[ "login #{user} #{pass}", "use sid=1",
		  "servernotifyregister event=textprivate",
		  "servernotifyregister event=channel id=1",
		  "servernotifyregister event=textchannel id=1",
		  "clientupdate client_nickname=#{Mambo.Helpers.escape(name)}" ]
		|> Enum.each(fn(x) -> send_to_server(s, x) end)
	end

	# --------------------
	# gen_sever callbacks
	# --------------------

	@doc false
	def init(s) do
		{:ok, socket} = :gen_tcp.connect(String.to_char_list!(s.host), s.port, [:binary])
		lc {m, a} inlist s.scripts, do: Mambo.EventManager.install_script(m, a)
		:ok = login(socket, s.name, s.user, s.pass)
		:erlang.send_after(300000, self(), :keep_alive)
		{:ok, {socket, s}}
	end

	@doc false
	def handle_call(:name, _, {_, s} = state) do
		{:reply, s.name, state}
	end

	@doc false
	def handle_call(_, _, state) do
		{:noreply, state}
	end

	@doc false
	def handle_cast({:send_msg, msg}, {s, _} = state) do
		cmd = "sendtextmessage targetmode=2 target=1 msg=#{Mambo.Helpers.escape(msg)}"
		:ok = send_to_server(s, cmd)
		{:noreply, state}
	end

	def handle_cast({:send_privmsg, {msg, cid}}, {s, _} = state) do
		cmd = "sendtextmessage targetmode=1 target=#{cid} msg=#{Mambo.Helpers.escape(msg)}"
		:ok = send_to_server(s, cmd)
		{:noreply, state}
	end

	def handle_cast({:send_gm, msg}, {s, _} = state) do
		:ok = send_to_server(s, "gm msg=#{Mambo.Helpers.escape(msg)}")
		{:noreply, state}
	end

	def handle_cast({:kick, {cid, msg}}, {s, _} = state) do
		cmd = "clientkick clid=#{cid} reasonid=5 reasonmsg=#{Mambo.Helpers.escape(msg)}"
		:ok = send_to_server(s, cmd)
		{:noreply, state}
	end

	def handle_cast({:ban, {cid, time, msg}}, {s, _} = state) do
		cmd = "banclient clid=#{cid} time=#{time} reasonmsg=#{Mambo.Helpers.escape(msg)}"
		:ok = send_to_server(s, cmd)
		{:noreply, state}
	end

	def handle_cast(_, state) do
		{:noreplay, state}
	end

	@doc false
	def handle_info({:tcp, _, <<@notify_msg, r :: binary>>}, {_, Settings[bot_id: bid]} = state) do
		{:ok, re} = Regex.compile("targetmode=([1-2]) msg=(\\S*)(?: target=[0-9]*)? " <>
		                          "invokerid=([0-9]*) invokername=(.*) invokeruid=(.*)", "i")

		case Regex.run(re, r) do
			[_, _, _, _, _, ^bid] ->
				{:noreply, state}

			[_, "1", msg, cid, name, uid] ->
				msg = Mambo.Helpers.unescape(msg)
				name = Mambo.Helpers.unescape(name)
				Mambo.EventManager.notify({:privmsg, {msg, name, {cid, uid}}})
				{:noreply, state}

			[_, "2", msg, cid, name, uid] ->
				msg = Mambo.Helpers.unescape(msg)
				name = Mambo.Helpers.unescape(name)
				Mambo.EventManager.notify({:msg, {msg, name, {cid, uid}}})
				{:noreply, state}

			_ ->
				{:noreply, state}
		end
	end

	def handle_info({:tcp, _, <<@notify_move, r :: binary>>}, state) do
		case Regex.run(%r/ctid=([0-9]*)/i, r) do
			[_, "1"] ->
				Mambo.EventManager.notify(:move_in)
				{:noreply, state}

			[_, _] ->
				Mambo.EventManager.notify(:move_out)
				{:noreply, state}

			_other ->
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

			_other ->
				{:noreply, state}
		end
	end

	def handle_info({:tcp_close, _}, state) do
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
