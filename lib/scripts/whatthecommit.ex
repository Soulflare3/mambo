defmodule Whatthecommit do
  @moduledoc """
  Prints a commit from http://whatthecommit.com/.

  Examples:
    .wtc
  """

  use GenEvent.Behaviour

  @doc false
  def init([]) do
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {".help wtc", _, {cid,_,_}}}, []) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
    {:ok, []}
  end

  @doc false
  def handle_event({:privmsg, {".help wtc", _, {clid,_}}}, []) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
    {:ok, []}
  end

  @doc false
  def handle_event({:msg, {".wtc", _, {cid,_,_}}}, []) do
    answer = fn(x) -> Mambo.Bot.send_msg(x, cid) end
    spawn(fn -> wtc(answer) end)
    {:ok, []}
  end

  @doc false
  def handle_event({:privmsg, {".wtc", _, {clid,_}}}, []) do
    answer = fn(x) -> Mambo.Bot.send_privmsg(x, clid) end
    spawn(fn -> wtc(answer) end)
    {:ok, []}
  end

  @doc false
  def handle_event(_, []) do
    {:ok, []}
  end

  # --------
  # Helpers
  # --------

  defp wtc(answer) do
    url = 'http://whatthecommit.com/index.txt'
    case :httpc.request(:get, {url, []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        answer.("#{String.strip(body)}")
      _ ->
        answer.("Something went wrong!")
    end
  end
end
