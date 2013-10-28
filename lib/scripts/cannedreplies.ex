defmodule Cannedreplies do
  @moduledoc """
  Mambo replies when certain keywords are written in the chat.

  Examples:
    cool
    gface
    goface
    edgyface
    dface
    ggface
    chownface
  """

  use GenEvent.Behaviour

  @responses [{"gface", "( ≖‿≖)"},
              {"cool", "COOL LIKE SNOWMAN ☃"},
              {"chownface", "( ´· ‿ ·`)"},
              {"goface", "ʕ ◔ϖ◔ʔ"},
              {"edgyface", "(ケ≖‿≖)ケ"},
              {"dface", "ಠ_ಠ"},
              {"ggface", "G_G"}]

  def init([]) do
    {:ok, []}
  end

  def handle_event({:msg, {".help replies", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".help replies", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
    {:ok, []}
  end

  def handle_event({:msg, {msg, _, {cid,_,_}}}, []) do
    case ListDict.get(@responses, msg) do
      nil ->
        {:ok, []}
      reply ->
        Mambo.Bot.send_msg(reply, cid)
        {:ok, []}
    end
  end

  def handle_event(_, []) do
    {:ok, []}
  end
end
