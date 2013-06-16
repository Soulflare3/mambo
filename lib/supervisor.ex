defmodule Tsmambo.Supervisor do
  use Supervisor.Behaviour

  def start_link() do
    case Tsmambo.Lib.consult("settings.cfg") do
      {:ok, settings} ->
        :supervisor.start_link(__MODULE__, settings)
      {:error, reason} ->
        IO.puts("Error in settings file: #{reason}")
    end
  end

  def init(settings) do
    children = [worker(Tsmambo.Client, [settings])]
    supervise(children, strategy: :one_for_one)
  end
end