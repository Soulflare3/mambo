defmodule Cannedreplies do
  use GenEvent.Behaviour

  @responses [{"gface", "( ≖‿≖)"},
              {"cool", "COOL LIKE SNOWMAN ☃"},
              {"chownface", "( ´· ‿ ·`)"},
              {"goface", "ʕ ◔ϖ◔ʔ"},
              {"edgyface", "(ケ≖‿≖)ケ"},
              {"dface", "ಠ_ಠ"}]

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, _userid}, state) do
    case msg do
      [key] ->
        case ListDict.get(@responses, key) do
          nil ->
            {:ok, state}
          reply ->
            :gen_server.cast(:mambo, {:send_txt, reply})
            {:ok, state}
        end
      _ ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
