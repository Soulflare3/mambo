defmodule Utils do
  use GenEvent.Behaviour

  @days   {'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'}
  @months {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
           'Oct', 'Nov', 'Dec'}

  defp date do
    {{year, month_num, day} = date, {h, m, s}} = :erlang.localtime()
    wday = elem(@days, :calendar.day_of_the_week(date) - 1)
    month = elem(@months, month_num - 1)
    :io_lib.format('~s ~s ~B ~2..0B:~2..0B:~2..0B ~B', [wday, month, day, h, m, s, year])
    |> List.flatten
    |> list_to_binary
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
