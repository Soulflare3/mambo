defmodule Cannedreplies do
  use GenEvent.Behaviour

  @responses [gface: "( ≖‿≖)",
              cool: "COOL LIKE SNOWMAN ☃",
              chownface: "( ´· ‿ ·`)",
              goface: "ʕ ◔ϖ◔ʔ",
              edgyface: "(ケ≖‿≖)ケ",
              dface: "ಠ_ಠ"]

  @id elem(Tsmambo.Lib.consult("settings.cfg"), 1)[:bot_id]

  def init(_args) do
    {:ok, []}
  end

  def handle_event({_, _, _, @id}, state) do
    {:ok, state}
  end

  def handle_event({msg, _user, _userid}, state) do
    case msg do
      [s] ->
        key = binary_to_atom(s)
        if Dict.has_key?(@responses, key) do
          :gen_server.cast(:mambo, {:send_txt, @responses[key]})
          {:ok, state}
        else
            {:ok, state}
        end
      _ ->
        {:ok, state}
    end
  end
end
