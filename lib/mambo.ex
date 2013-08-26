defmodule Mambo do
	@moduledoc """
	Entry point of the bot.
	"""

	use Application.Behaviour

	@doc """
	Starts the bot application.
	"""
	@spec start(:normal | {:takeover, node} | {:failover, node}, []) :: {:ok, pid}
	def start(_, []) do
		{:ok, _} = Mambo.Supervisor.start_link()
	end

	@doc """
	Stops the bot application.
	"""
	@spec stop() :: :ok
	def stop() do
		:ok
	end
end
