defmodule Cannedreplies do
  use GenEvent.Behaviour

  @responses [gface: "( ≖‿≖)",
              cool: "COOL LIKE SNOWMAN ☃",
              chownface: "( ´· ‿ ·`)",
              goface: "ʕ ◔ϖ◔ʔ",
              edgyface: "(ケ≖‿≖)ケ",
              dface: "ಠ_ಠ"]

  @name elem(Tsmambo.Lib.consult("settings.cfg"), 1)[:name]

  def init(_args) do
    {:ok, []}
  end

  def handle_event({_, _, @name}, state) do
    {:ok, state}
  end

  def handle_event({gen_server, msg, _user}, state) do
    case msg do
      [s] ->
        key = binary_to_atom(s)
        if Dict.has_key?(@responses, key) do
          :gen_server.cast(gen_server, {:send_txt, @responses[key]})
          {:ok, state}
        else
            {:ok, state}
        end
      _ ->
        {:ok, state}
    end
  end
end
