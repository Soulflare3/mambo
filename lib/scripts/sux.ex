defmodule Sux do
	@moduledoc """
	Curse something.

	Examples:
	  .sux <word> | <phrase>
	"""

	use GenEvent.Behaviour

	@doc false
	def init([]) do
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {".help sux", _, _}}, []) do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {".help sux", _, {id, _}}}, []) do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {<<".sux ", msg :: binary>>, _, _}}, []) do
		msg |> sux |> Mambo.Bot.send_msg
		{:ok, []}
	end

	@doc false
	def handle_event(_, []) do
		{:ok, []}
	end

	# --------
	# Helpers
	# --------

	defp sux(what) do
		"fuck #{what}; #{what} sucks; #{what} is dying; #{what} is dead to me; #{what} hit wtc;"
	end
end
