defmodule Mambo.Supervisor do
	@moduledoc """
	Main bot supervisor, it supervises a `Mambo.Bot` process.
	"""

	use Supervisor.Behaviour

	@doc """
	Starts the supervisor. Returns `{:ok, pid}` on success.
	"""
	@spec start_link() :: {:ok, pid}
	def start_link() do
		{:ok, _} = :supervisor.start_link(__MODULE__, [])
	end

	@doc false
	def init([]) do
		children = [worker(Mambo.Brain, []),
			        worker(Mambo.EventManager, []),
		            worker(Mambo.Bot, [])]
		supervise(children, strategy: :one_for_all)
	end
end
