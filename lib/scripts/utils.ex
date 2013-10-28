defmodule Utils do
  @moduledoc """
  Utility commands.

  Examples:
    .ping
    .date
    .time
    .uptime
    .version
  """

  use GenEvent.Behaviour

  @days {'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'}
  @months {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
           'Oct', 'Nov', 'Dec'}

  def init([]) do
    {:ok, []}
  end

  def handle_event({:msg, {".help utils", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".help utils", _, {clid, _}}}, []) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
    {:ok, []}
  end

  def handle_event({:msg, {".ping", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg("pong", cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".ping", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg("pong", clid)
    {:ok, []}
  end

  def handle_event({:msg, {".date", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(date(), cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".date", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(date(), clid)
    {:ok, []}
  end

  def handle_event({:msg, {".time", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(date(), cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".time", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(date(), clid)
    {:ok, []}
  end

  def handle_event({:msg, {".uptime", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(uptime(), cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".uptime", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(uptime(), clid)
    {:ok, []}
  end

  def handle_event({:msg, {".version", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(version(), cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".version", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(version(), clid)
    {:ok, []}
  end

  def handle_event(_, []) do
    {:ok, []}
  end

  # Helpers

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

  defp version() do
    if Enum.all?(:application.which_applications(), fn({a,_,_}) -> a != :mix end) do
      Mix.loadpaths()
    end
    "Mambo - #{Mix.project()[:version]}"
  end
end
