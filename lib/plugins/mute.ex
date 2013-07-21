defmodule Mute do
  use GenEvent.Behaviour

  @admins elem(Tsmambo.Lib.consult("settings.cfg"), 1)[:admins]

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, userid, _talk}, state) when userid in @admins do
    case msg do
      ["!mute"] ->
        :gen_server.cast(:mambo, :mute)
        :gen_server.cast(:mambo, {:send_txt, "Muted."})
        {:ok, state}
      ["!unmute"] ->
        :gen_server.cast(:mambo, :unmute)
        :gen_server.cast(:mambo, {:send_txt, "Unmuted."})
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
