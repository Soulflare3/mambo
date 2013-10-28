defmodule Admin do
  @moduledoc """
  Admin script.

  Examples
    .mute
    .unmute
    .gm <message>
  """

  use GenEvent.Behaviour

  def init([]) do
    {:ok, []}
  end

  def handle_event({:msg, {".help admin", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".help admin", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
    {:ok, []}
  end

  def handle_event({:msg, {".mute", _, {cid,_,uid}}}, []) do
    if uid in Mambo.Bot.admins() do
      Mambo.Bot.mute(cid)
      Mambo.Bot.send_msg("Muted.", cid)
    end
    {:ok, []}
  end

  def handle_event({:msg, {".unmute", _, {cid,_,uid}}}, []) do
    if uid in Mambo.Bot.admins() do
      Mambo.Bot.unmute(cid)
      Mambo.Bot.send_msg("Unmuted.", cid)
    end
    {:ok, []}
  end

  def handle_event({type, {<<".gm ", msg :: binary>>, _, ids}}, []) do
    uid = case type do
      :msg -> elem(ids, 2)
      :privmsg -> elem(ids, 1)
    end
    if uid in Mambo.Bot.admins() do
      Mambo.Bot.send_gm(msg)
    end
    {:ok, []}
  end

  def handle_event(_, []) do
    {:ok, []}
  end
end
