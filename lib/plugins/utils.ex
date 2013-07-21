defmodule Utils do
  use GenEvent.Behaviour

  defp week_day(num) do
    case num do
      1 -> "Mon"
      2 -> "Tue"
      3 -> "Wed"
      4 -> "Thu"
      5 -> "Fri"
      6 -> "Sat"
      7 -> "Sun"
    end
  end

  defp month_name(num) do
    case num do
      1  -> "Jan"
      2  -> "Feb"
      3  -> "Mar"
      4  -> "Apr"
      5  -> "May"
      6  -> "Jun"
      7  -> "Jul"
      8  -> "Aug"
      9  -> "Sep"
      10 -> "Oct"
      11 -> "Nov"
      12 -> "Dec"
    end
  end

  defp date do
    {{year, month_num, day} = date, {h, m, s}} = :erlang.localtime()
    wday = :calendar.day_of_the_week(date) |> week_day
    month = month_name(month_num)
    "#{wday} #{month} #{day} #{h}:#{m}:#{s} #{year}"
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({["!time"], _user, _userid, :unmuted}, state) do
    :gen_server.cast(:mambo, {:send_txt, date})
    {:ok, state}
  end

  def handle_event({["!date"], _user, _userid, :unmuted}, state) do
    :gen_server.cast(:mambo, {:send_txt, date})
    {:ok, state}
  end

  def handle_event({["ping"], _user, _userid, :unmuted}, state) do
    :gen_server.cast(:mambo, {:send_txt, "pong"})
    {:ok, state}
  end

  def handle_event({["!setnick", nick], _user, _userid, :unmuted}, state) do
    :gen_server.cast(:mambo, {:change_nick, nick})
    :gen_server.cast(:mambo, {:send_txt, "My name is now [b]#{nick}[/b]."})
    {:ok, state}
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
