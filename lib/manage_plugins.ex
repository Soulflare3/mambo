defmodule Plugins.Manager do
  use GenEvent.Behaviour

  @admins elem(Tsmambo.Lib.consult("settings.cfg"), 1)[:admins]

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, userid, _talk}, state) do
    case msg do
      ["!load", plugin] when userid in @admins ->
        :gen_server.cast(:mambo, {:load_plugin, binary_to_atom(plugin)})
        {:ok, state}
      ["!unload", plugin] when userid in @admins ->
        :gen_server.cast(:mambo, {:unload_plugin, binary_to_atom(plugin)})
        {:ok, state}
      ["!list"] when userid in @admins ->
        :gen_server.cast(:mambo, :list_plugins)
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
