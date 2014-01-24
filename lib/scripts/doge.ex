defmodule Doge do
  @moduledoc """
  Dogecoin prices.

  Examples:
    .doge
  """

  @url "http://dogechain.info/chain/Dogecoin/q/last_price"

  use GenEvent.Behaviour

  def init(_) do
    {:ok, []}
  end

  def handle_event({:msg, {".help doge", _, {cid,_,_}}}, _) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
    {:ok, []}
  end

  def handle_event({:privmsg, {".help doge", _, {clid,_}}}, _) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
    {:ok, []}
  end

  def handle_event({:msg, {".doge", _, {cid,_,_}}}, _) do
    answer = fn(x) -> Mambo.Bot.send_msg(x, cid) end
    spawn(fn -> get_prices(answer) end)
    {:ok, []}
  end

  def handle_event({:privmsg, {".doge", _, {clid,_}}}, _) do
    answer = fn(x) -> Mambo.Bot.send_privmsg(x, clid) end
    spawn(fn -> get_prices(answer) end)
    {:ok, []}
  end

  def handle_event(_, _) do
    {:ok, []}
  end

  # Helpers

  defp get_prices(answer) do
    case :hackney.get(@url, [], <<>>, []) do
      {:ok, 200, _, client} ->
        {:ok, body} = :hackney.body(client)
        prices = :jsx.decode(body)
        answer.(<<"\n[b]Last:[/b] #{prices["last"]} [b]BTC[/b]\n",
        "[b]High:[/b] #{prices["high"]} [b]BTC[/b]\n",
        "[b]Low:[/b]  #{prices["low"]} [b]BTC[/b]\n",
        "[b]Avg:[/b]  #{prices["avg"]} [b]BTC[/b]\n">>)
      _ ->
        answer.("Something went wrong.")
    end
  end
end
