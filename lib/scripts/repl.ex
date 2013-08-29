## To run this script you will need to install docker and create an elixir
## image. For more information check https://github.com/mrshankly/mambo_ex.

defmodule Repl do
	@moduledoc """
	Starts an elixir repl in a private chat.

	Examples
	  mambo elixir
	"""

	use GenEvent.Behaviour

	@doc false
	def init(args) do
		{:ok, args}
	end

	@doc false
	def handle_event({:msg, {"help elixir", _, _}}, state) do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, state}
	end

	@doc false
	def handle_event({:privmsg, {"help elixir", _, {id, _}}}, state) do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, state}
	end

	@doc false
	def handle_event({type, {msg, _, {id, uid}}}, [u, p, h] = state)
	                  when type == :msg or type == :privmsg do

		if Regex.match?(%r/^(?:#{Mambo.Bot.name} )?elixir$/i, msg) do
			c = Mambo.Brain.get("elixir" <> uid)
			cmd = "docker run -c=100 -m=26214400 -d elixir ./mambo_ex.sh " <>
			      "-u #{u} -p #{p} -h #{h} -b #{Mambo.Bot.id} -n #{uid} -c #{id} " <>
			      "2> /dev/null"

			container = start_c(c, cmd)
			true = Mambo.Brain.set({"elixir" <> uid, container})
			{:ok, state}
		else
			{:ok, state}
		end
	end

	@doc false
	def handle_event(_, state) do
		{:ok, state}
	end

	# --------
	# Helpers
	# --------

	defp start_c(c, cmd) do
		if c in list_c do
			System.cmd("docker kill #{c}")
		end

		String.strip(System.cmd(cmd))
	end


	defp list_c() do
		String.split(System.cmd("docker ps -q"), "\n")
	end
end
