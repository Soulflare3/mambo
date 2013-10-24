defmodule Mambo.WatcherSup do
  @moduledoc false

  use Supervisor.Behaviour

  @watcher_sup __MODULE__

  # API.

  @spec start_link() :: {:ok, pid()}
  def start_link() do
    :supervisor.start_link({:local, @watcher_sup}, __MODULE__, [])
  end

  @spec add_watcher([any()]) :: {:ok, pid()}
  def add_watcher(args) do
    :supervisor.start_child(@watcher_sup, args)
  end

  # Supervisor callbacks.

  def init([]) do
    supervise([worker(Mambo.Watcher, [])], strategy: :simple_one_for_one)
  end
end
