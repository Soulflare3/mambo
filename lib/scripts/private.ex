defmodule Private do
  @moduledoc """
  Starts a private chat with mambo.

  Examples:
    .talk
    .speak
    .chat
  """

  use GenEvent.Behaviour

  @cmds [".talk", ".speak", ".chat"]

  # --------------------
  # gen_event callbacks
  # --------------------

  @doc false
  def init([]) do
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {<<".help ", r :: binary>>, _, {cid,_,_}}}, []) do
    if r in ["talk", "speak", "chat"] do
      Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
      {:ok, []}
    else
      {:ok, []}
    end
  end

  @doc false
  def handle_event({:privmsg, {<<".help ", r :: binary>>, _, {clid,_}}}, []) do
    if r in ["talk", "speak", "chat"] do
      Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
      {:ok, []}
    else
      {:ok, []}
    end
  end

  @doc false
  def handle_event({:msg, {msg, _, {_,clid,_}}}, []) when msg in @cmds do
    Mambo.Bot.send_privmsg("Hello.", clid)
    {:ok, []}
  end

  @doc false
  def handle_event(_, []) do
    {:ok, []}
  end
end
