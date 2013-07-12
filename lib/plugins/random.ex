defmodule Random do
  use GenEvent.Behaviour

  defp attack() do
    Enum.at ["rock", "paper", "scissors"], :random.uniform(3) - 1
  end

  defp winner(moves) do
    case moves do
      {"rock", "scissors"} -> :win
      {"paper", "rock"} -> :win
      {"scissors", "paper"} -> :win
      {same, same} -> :draw
      {_, _} -> :lose
    end
  end

  defp pp_rps(p1, p2) do
    case winner({p1, p2}) do
      :win ->
        "I choose #{p2}. You win!"
      :draw ->
        "I choose #{p2}. It's a draw."
      :lose ->
        "I choose #{p2}. I WIN!"
    end
  end

  defp roll do
    integer_to_binary(:random.uniform(100))
  end

  ## gen_event callbacks

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, _userid}, state) do
    case msg do
      ["!rock"] ->
        :gen_server.cast(:mambo, {:send_txt, "#{pp_rps("rock", attack())}"})
        {:ok, state}
      ["!paper"] ->
        :gen_server.cast(:mambo, {:send_txt, "#{pp_rps("paper", attack())}"})
        {:ok, state}
      ["!scissors"] ->
        :gen_server.cast(:mambo, {:send_txt, "#{pp_rps("scissors", attack())}"})
        {:ok, state}
      ["!roll"] ->
        :gen_server.cast(:mambo, {:send_txt, "#{roll()}"})
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
