defmodule Uptime do
  use GenEvent.Behaviour

  defp uptime() do
    {total, _} = :erlang.statistics(:wall_clock)
    {d, {h, m, s}} = :calendar.seconds_to_daystime(div(total, 1000))
    "#{d} days, #{h} hours, #{m} minutes and #{s} seconds."
  end

  ## gen_event callbacks

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, _userid, :unmuted}, state) do
    case msg do
      ["!uptime"] ->
        :gen_server.cast(:mambo, {:send_txt, uptime()})
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
