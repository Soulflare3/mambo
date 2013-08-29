defmodule Cannedreplies do
	@moduledoc """
	Mambo replies when certain keywords are written in the chat.

	Examples:
	  cool
	  gface
	  goface
	"""

	use GenEvent.Behaviour

	@responses [{"gface", "( ≖‿≖)"},
	            {"cool", "COOL LIKE SNOWMAN ☃"},
	            {"chownface", "( ´· ‿ ·`)"},
	            {"goface", "ʕ ◔ϖ◔ʔ"},
	            {"edgyface", "(ケ≖‿≖)ケ"},
	            {"dface", "ಠ_ಠ"},
	            {"ggface", "G_G"}]

	@doc false
	def init([]) do
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"help cannedreplies", _, _}}, []) do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"help cannedreplies", _, {id, _}}}, []) do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {msg, _, _}}, []) do
		case ListDict.get(@responses, msg) do
			nil ->
				{:ok, []}
			reply ->
				Mambo.Bot.send_msg(reply)
				{:ok, []}
		end
	end

	@doc false
	def handle_event(_, []) do
		{:ok, []}
	end
end
