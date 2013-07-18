defmodule Fortune do
  use GenEvent.Behaviour

  defp get_fortune(fortunes) do
      i = :random.uniform(1615)
      f = Enum.at fortunes, i
      "[b]Fortune:[/b] #{f}"
  end

  def init(_args) do
    {:ok, ff} = File.read "fortunes"
    ff = String.split ff, "\n"
    {:ok, ff}
  end

  def handle_event({msg, _user, _userid}, fortunes) do
    case msg do
      ["!fortune"] ->
        :gen_server.cast(:mambo, {:send_txt, get_fortune(fortunes)})
        {:ok, fortunes}
       _ ->
        {:ok, fortunes}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
