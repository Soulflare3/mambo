defmodule Plugins.Manager do
    use GenEvent.Behaviour

    @admins elem(Tsmambo.Lib.consult("settings.cfg"), 1)[:admins]

    def init(_args) do
        {:ok, []}
    end

    def handle_event({gen_server, msg, _user, userid}, state) do
        case msg do
            ["!load", plugin] when userid in @admins ->
                :gen_server.cast(gen_server, {:load_plugin, binary_to_atom(plugin)})
                {:ok, state}
            ["!unload", plugin] when userid in @admins ->
                :gen_server.cast(gen_server, {:unload_plugin, binary_to_atom(plugin)})
                {:ok, state}
            ["!list"] when userid in @admins ->
                :gen_server.cast(gen_server, :list_plugins)
                {:ok, state}
            _ ->
                {:ok, state}
        end
    end
end