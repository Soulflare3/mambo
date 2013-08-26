defmodule Private do
	@moduledoc """
	Starts a private chat with mambo.

	Examples:
	  mambo
	  mambo talk to me
	"""

	use GenEvent.Behaviour

	# --------------------
	# gen_event callbacks
	# --------------------

	@doc false
	def init([]) do
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {<<"help ", r :: binary>>, _, _}}, []) do
		if r in ["talk", "speak", "chat"] do
			Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
			{:ok, []}
		else
			{:ok, []}
		end
	end

	def handle_event({:privmsg, {<<"help ", r :: binary>>, _, {id, _}}}, []) do
		if r in ["talk", "speak", "chat"] do
			Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
			{:ok, []}
		else
			{:ok, []}
		end
	end

	def handle_event({:msg, {msg, _, {id, _}}}, []) do
		if Regex.match?(%r/^#{Mambo.Bot.name}( (talk|speak|chat)?(( (with|to))? me)?)?$/i, msg) do
			Mambo.Bot.send_privmsg("Hello.", id)
			{:ok, []}
		else
			{:ok, []}
		end
	end

	def handle_event(_, []) do
		{:ok, []}
	end
end
