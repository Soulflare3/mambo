defmodule Sux do
  use GenEvent.Behaviour

  defp sux(what) do
    "fuck #{what}; #{what} sucks; #{what} is dying; #{what} is dead to me; #{what} hit wtc"
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({gen_server, msg, _user, _userid}, state) do
    case msg do
      ["!sux", what] ->
        :gen_server.cast(gen_server, {:send_txt, sux(what)})
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end
end
