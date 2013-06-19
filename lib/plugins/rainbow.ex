defmodule Rainbow do
  use GenEvent.Behaviour

  @colors ["#FF0000",
           "#FF8000",
           "#EAB700",
           "#008000",
           "#0000FF",
           "#4B0082",
           "#9400D3"]

  defp rainbow(s) do
    rainbow(String.codepoints(s), @colors, [])
  end

  defp format(color, text) do
    "[color=#{color}]#{text}[/color]"
  end

  defp rainbow([], _, acc) do
    "#{Enum.reverse(acc)}"
  end

  defp rainbow(text, [], acc) do
    rainbow(text, @colors, acc)
  end

  defp rainbow([" " | rest], colors, acc) do
    rainbow(rest, colors, [" " | acc])
  end

  defp rainbow([h | rest], [c | nc], acc) do
    rainbow(rest, nc, [format(c, h) | acc])
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, _userid}, state) do
    case msg do
      ["!gay", s] ->
        :gen_server.cast(:mambo, {:send_txt, rainbow(s)})
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end
end
