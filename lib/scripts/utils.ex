defmodule Utils do
  @moduledoc """
  Utility commands.

  Examples:
    .ping
    .date
    .time
    .uptime
  """

  use GenEvent.Behaviour

  @days {'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'}
  @months {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
           'Oct', 'Nov', 'Dec'}

  @doc false
  def init([]) do
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {".help utils", _, _}}, []) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
    {:ok, []}
  end

  @doc false
  def handle_event({:privmsg, {".help utils", _, {id, _}}}, []) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {".ping", _, _}}, []) do
    Mambo.Bot.send_msg("pong")
    {:ok, []}
  end

  @doc false
  def handle_event({:privmsg, {".ping", _, {id, _}}}, []) do
    Mambo.Bot.send_privmsg("pong", id)
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {".date", _, _}}, []) do
    Mambo.Bot.send_msg(date)
    {:ok, []}
  end

  @doc false
  def handle_event({:privmsg, {".date", _, {id, _}}}, []) do
    Mambo.Bot.send_privmsg(date, id)
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {".time", _, _}}, []) do
    Mambo.Bot.send_msg(date)
    {:ok, []}
  end

  @doc false
  def handle_event({:privmsg, {".time", _, {id, _}}}, []) do
    Mambo.Bot.send_privmsg(date, id)
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {".uptime", _, _}}, []) do
    Mambo.Bot.send_msg(uptime)
    {:ok, []}
  end

  @doc false
  def handle_event({:privmsg, {".uptime", _, {id, _}}}, []) do
    Mambo.Bot.send_privmsg(uptime, id)
    {:ok, []}
  end

  @doc false
  def handle_event(_, []) do
    {:ok, []}
  end

  # --------
  # Helpers
  # --------

  defp date() do
    {{year, month_num, day} = date, {h, m, s}} = :erlang.localtime()
    wday = elem(@days, :calendar.day_of_the_week(date) - 1)
    month = elem(@months, month_num - 1)
    :io_lib.format('~s ~s ~B ~2..0B:~2..0B:~2..0B ~B', [wday, month, day, h, m, s, year])
    |> List.flatten
    |> String.from_char_list!
  end

  defp uptime() do
    {total, _} = :erlang.statistics(:wall_clock)
    {d, {h, m, s}} = :calendar.seconds_to_daystime(div(total, 1000))
    "#{d} days, #{h} hours, #{m} minutes and #{s} seconds."
  end
end
