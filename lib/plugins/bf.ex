defmodule Brainfuck do
	# https://en.wikipedia.org/wiki/Brainfuck

	#  >  move the pointer to the rigth
	#  <  move the pointer to the left
	#  +  increment the byte at the data pointer
	#  -  decrement the byte at the data pointer
	#  ,  accept one byte of input and store it at the data pointer
	#  .  output the byte at the data pointer

	#  [  if the data which the data pointer is pointing at is 0, jump forward
	#     to the command after the matching square bracket. Otherwise, just
	#     continue to the next command

	#  ]  if the data which the data pointer is pointing at is not 0, jump
	#     backwards to the command after the matching square bracket. Otherwise,
	#     just continue to the next command

	use GenEvent.Behaviour

	@tape <<0 :: [size(30000), unit(8)]>>
	@data_pointer 0

	defp run(instructions) do
		{_, _, out} = parse(instructions, @data_pointer, @tape, [])
		Enum.join(Enum.reverse(out), "")
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

	# gen_event callbacks
	def init(_args) do
		{:ok, []}
	end

	def handle_event({msg, _user, _userid}, state) do
		case msg do
			["!bf", instructions] ->
				out = run(instructions)
				:gen_server.cast(:mambo, {:send_txt, "[b]Brainfuck:[/b] #{out}"})
				{:ok, state}
			_ ->
				{:ok, state}
		end
	end
end
