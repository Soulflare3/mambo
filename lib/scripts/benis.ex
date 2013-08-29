defmodule Benis do
	@moduledoc """
	Benisify a sentence.

	Examples:
	  !benis <sentence>
	"""

	use GenEvent.Behaviour

	def init([]) do
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"help benis", _, _}}, []) do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"help benis", _, {id, _}}}, []) do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {<<"!benis ", msg :: binary>>, _, _}}, []) do
		msg |> benisify |> Mambo.Bot.send_msg
		{:ok, []}
	end

	@doc false
	def handle_event(_, []) do
		{:ok, []}
	end

	# --------
	# Helpers
	# --------

	defp benisify(s) do
		Enum.reduce([String.downcase(&1),
			Regex.replace(%r/x/, &1, "cks"),
			Regex.replace(%r/ing/, &1, "in"),
			Regex.replace(%r/you/, &1, "u"),
			Regex.replace(%r/oo/, &1, String.duplicate("u", :random.uniform(5))),
			Regex.replace(%r/ck/, &1, String.duplicate("g", :random.uniform(5))),
			Regex.replace(%r/(t+)(?=[aeiouys]|\b)/, &1, String.duplicate("d", String.length("&") + 1)),
			Regex.replace(%r/p/, &1, "b"),
			Regex.replace(%r/\bthe\b/, &1, "da"),
			Regex.replace(%r/\bc/, &1, "g"),
			Regex.replace(%r/\bis/, &1, "are"),
			Regex.replace(%r/c+(?![eiy])/, &1, String.duplicate("g", :random.uniform(5))),
			Regex.replace(%r/k+(?=[aeiouy]|\b)/, &1, String.duplicate("g", :random.uniform(5))),
			Regex.replace(%r/([?!.]|$)+/, &1, (String.duplicate("&", :random.uniform(5))) <> " " <> ":DD:DDD:D:DD")],
			s,
			fn(f, acc) -> f.(acc) end
		)
	end
end
