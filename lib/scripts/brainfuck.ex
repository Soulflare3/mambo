defmodule Brainfuck do
	@moduledoc """
	Brainfuck interpreter.

	Examples
	  !bf ++++++++++[>++++++++>++++++<<-]>---.>+++++.<.>+.<++.
	"""

	use GenEvent.Behaviour

	@tape <<0 :: [size(30000), unit(8)]>>
  	@data_pointer 0

	def init([]) do
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"help brainfuck", _, _}}, []) do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"help brainfuck", _, {id, _}}}, []) do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {<<"!bf ", l :: binary>>, _, _}}, []) do
		answer = fn(x) -> Mambo.Bot.send_msg(x) end
		spawn(fn -> run(l, answer) end)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {<<"!bf ", l :: binary>>, _, {id, _}}}, []) do
		answer = fn(x) -> Mambo.Bot.send_privmsg(x, id) end
		spawn(fn -> run(l, answer) end)
		{:ok, []}
	end

	@doc false
	def handle_event(_, []) do
		{:ok, []}
	end

	# --------
	# Helpers
	# --------

	def run(instructions, callback) do
		{_, _, out} = parse(instructions, @data_pointer, @tape, [])
		callback.("#{Enum.join(Enum.reverse(out), "")}")
	end

	# parse brainfuck code
	defp parse(<< ?>, ins :: binary>>, dpointer, tape, out) do
		parse(ins, dpointer + 1, tape, out)
	end

	defp parse(<< ?<, ins :: binary>>, dpointer, tape, out) do
		parse(ins, dpointer - 1, tape, out)
	end

	defp parse(<< ?+, ins :: binary>>, dpointer, tape, out) do
		<<prev :: [binary, size(dpointer)], x :: integer, next :: binary>> = tape
		parse(ins, dpointer, <<prev :: binary, (x + 1) :: integer, next :: binary>>, out)
	end

	defp parse(<< ?-, ins :: binary>>, dpointer, tape, out) do
		<<prev :: [binary, size(dpointer)], x :: integer, next :: binary>> = tape
		parse(ins, dpointer, <<prev :: binary, (x - 1) :: integer, next :: binary>>, out)
	end

	# user input is disabled
	defp parse(<< ?,, ins :: binary>>, dpointer, tape, out) do
		parse(ins, dpointer, tape, out)
	end

	defp parse(<< ?., ins :: binary>>, dpointer, tape, out) do
		<<_ :: [binary, size(dpointer)], x :: integer, _ :: binary>> = tape
		parse(ins, dpointer, tape, [<<x>> | out])
	end

	defp parse(<< ?[, ins :: binary>>, dpointer, tape, out) do
		match = find_matching(ins)
		<<bf_loop :: [binary, size(match)], next :: binary>> = ins

		<<_ :: [binary, size(dpointer)], x :: integer, _ :: binary>> = tape

		if x === 0 do
			parse(next, dpointer, tape, out)
		else
			{ndpointer, ntape, nout} = parse(bf_loop, dpointer, tape, out)
			parse(<< ?[, bf_loop :: binary, ?], next :: binary>>, ndpointer, ntape, nout)
		end
	end

	defp parse(<< _, ins :: binary>>, dpointer, tape, out) do
		parse(ins, dpointer, tape, out)
	end

	defp parse(<<>>, dpointer, tape, out) do
		{dpointer, tape, out}
	end

	# find matching bracket
	defp find_matching(ins) do
		find_matching(ins, 0, 0)
	end

	defp find_matching(<<?], _ :: binary>>, 0, pos) do
		pos + 1
	end

	defp find_matching(<<?], ins :: binary>>, n, pos) do
		find_matching(ins, n - 1, pos + 1)
	end

	defp find_matching(<<?[, ins :: binary>>, n, pos) do
		find_matching(ins, n + 1, pos + 1)
	end

	defp find_matching(<<_, ins :: binary>>, n, pos) do
		find_matching(ins, n, pos + 1)
	end
end
