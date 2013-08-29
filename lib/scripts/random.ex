defmodule Random do
	@moduledoc """
	Luck games.

	Examples:
	  !roll
	  !rock
	  !paper
	  !scissors
	"""

	use GenEvent.Behaviour

	@doc false
	def init([]) do
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"help random", _, _}}, []) do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"help random", _, {id, _}}}, []) do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"!roll", _, _}}, []) do
		Mambo.Bot.send_msg(roll)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"!roll", _, {id, _}}}, []) do
		Mambo.Bot.send_privmsg(roll, id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"!rock", _, _}}, []) do
		rps("rock", attack) |> Mambo.Bot.send_msg
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"!rock", _, {id, _}}}, []) do
		rps("rock", attack) |> Mambo.Bot.send_privmsg(id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"!paper", _, _}}, []) do
		rps("paper", attack) |> Mambo.Bot.send_msg
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"!paper", _, {id, _}}}, []) do
		rps("paper", attack) |> Mambo.Bot.send_privmsg(id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"!scissors", _, _}}, []) do
		rps("scissors", attack) |> Mambo.Bot.send_msg
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"!scissors", _, {id, _}}}, []) do
		rps("scissors", attack) |> Mambo.Bot.send_privmsg(id)
		{:ok, []}
	end

	@doc false
	def handle_event(_, []) do
		{:ok, []}
	end

	# --------
	# Helpers
	# --------

	defp roll() do
		integer_to_binary(:random.uniform(100))
	end

	defp rps(p1, p2) do
		case winner({p1, p2}) do
			:win  -> "I choose #{p2}. You win!"
			:draw -> "I choose #{p2}. It's a draw."
			:lose -> "I choose #{p2}. I WIN!"
		end
	end

	defp winner(moves) do
		case moves do
			{"rock", "scissors"} -> :win
			{"paper", "rock"} -> :win
			{"scissors", "paper"} -> :win
			{same, same} -> :draw
			{_, _} -> :lose
		end
	end

	defp attack() do
		Enum.at ["rock", "paper", "scissors"], :random.uniform(3) - 1
	end
end
