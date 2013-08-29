defmodule Rainbow do
	@moduledoc """
	Rainbow color a phrase.

	Examples:
	  !gay <phrase>
	"""

	use GenEvent.Behaviour

	@colors ["#FF0000",
			 "#FF8000",
			 "#EAB700",
			 "#008000",
			 "#0000FF",
			 "#4B0082",
			 "#9400D3"]

	@doc false
	def init([]) do
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {<<"help ", c :: binary>>, _, _}}, [])
	                 when c == "gay" or c == "rainbow" do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {<<"help ", c :: binary>>, _, {id, _}}}, [])
	                 when c == "gay" or c == "rainbow" do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {<<"!gay ", msg :: binary>>, _, _}}, []) do
		msg |> rainbow |> Mambo.Bot.send_msg
		{:ok, []}
	end

	@doc false
	def handle_event(_, []) do
		{:ok, []}
	end

	# --------
	# Helpers
	# --------

	defp rainbow(s) do
		rainbow(String.codepoints(s), @colors, [])
	end

	defp rainbow([], _, acc) do
		"#{Enum.reverse(acc)}"
	end

	defp rainbow(text, [], acc) do
		rainbow(text, @colors, acc)
	end

	defp rainbow([" " | rest], colors, acc) do
		rainbow(rest, colors, [" " | acc])
	end

	defp rainbow([h | rest], [c | nc], acc) do
		rainbow(rest, nc, [format(c, h) | acc])
	end

	defp format(color, text) do
		"[color=#{color}]#{text}[/color]"
	end
end
