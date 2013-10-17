defmodule Calculator do
	@moduledoc """
	I'm a mathemagician yo!

	Examples
	  mambo calc <expression>
	  mambo convert me <expression> to <units>
	"""

	use GenEvent.Behaviour

	@doc false
	def init([]) do
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"help math", _, _}}, []) do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"help math", _, {id, _}}}, []) do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {msg, _, _}}, []) do
		case Regex.run(%r/(calc|calculate|calculator|convert|math|maths)( me)? (.*)/i, msg, capture: [3]) do
			[q] ->
				answer = fn(x) -> Mambo.Bot.send_msg(x) end
				spawn(fn -> calc(q, answer) end)
				{:ok, []}
			_ ->
				{:ok, []}
		end
	end

	@doc false
	def handle_event({:privmsg, {msg, _, {id, _}}}, []) do
		case Regex.run(%r/(calc|calculate|calculator|convert|math|maths)( me)? (.*)/i, msg, capture: [3]) do
			[q] ->
				answer = fn(x) -> Mambo.Bot.send_privmsg(x, id) end
				spawn(fn -> calc(q, answer) end)
				{:ok, []}
			_ ->
				{:ok, []}
		end
	end

	@doc false
	def handle_event(_, []) do
		{:ok, []}
	end

	# --------
	# Helpers
	# --------

	defp calc(q, answer) do
		url = 'https://www.google.com/ig/calculator?hl=en&q=#{URI.encode(q)}'

		case :httpc.request(:get, {url, []}, [], body_format: :binary) do
			{:ok, {{_, 200, _}, _, body}} ->
				json = good_json(body)
				{:ok, data} = JSEX.decode(json)

				if data["error"] == "" do
					answer.(data["rhs"])
				else
					answer.("No result.")
				end
			_ ->
				answer.("Something went wrong.")
		end
	end

	defp good_json(bad_json) do
		bad_json
			|> String.replace("lhs", "\"lhs\"")
			|> String.replace("rhs", "\"rhs\"")
			|> String.replace("error", "\"error\"")
			|> String.replace("icc", "\"icc\"")
	end
end
