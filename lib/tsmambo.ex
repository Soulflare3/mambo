defmodule Tsmambo do
    use Application.Behaviour

    def start(_type, _args) do
        Tsmambo.Supervisor.start_link()
    end

    def stop() do
        :ok
    end
end
