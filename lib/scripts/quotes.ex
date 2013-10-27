defmodule Quotes do
  @moduledoc """
  Quotes script.

  Examples
    .quote add <phrase>
    .quote find <query>
    .quote rm <id>
    .quote <id>
    .quote
  """

  use GenEvent.Behaviour

  def init([]) do
    {:ok, Mambo.Brain.quotes_max()}
  end

  def handle_event({:msg, {<<".quote add ", content :: binary>>, _, {cid,_,_}}}, max) do
    case add_quote(max + 1, content) do
      {:ok, id} ->
        Mambo.Bot.send_msg("Quote [b]#{id}[/b] added.", cid)
        {:ok, id}
      :error ->
        Mambo.Bot.send_msg("Did you forget to enter a quote?", cid)
        {:ok, max}
    end
  end

  def handle_event({:privmsg, {<<".quote add ", content :: binary>>, _, {clid,_}}}, max) do
    case add_quote(max + 1, content) do
      {:ok, id} ->
        Mambo.Bot.send_privmsg("Quote [b]#{id}[/b] added.", clid)
        {:ok, id}
      :error ->
        Mambo.Bot.send_privmsg("Did you forget to enter a quote?", clid)
        {:ok, max}
    end
  end

  def handle_event({:msg, {<<".quote find ", query :: binary>>, _, {cid,_,_}}}, max) do
    answer = fn(x) -> Mambo.Bot.send_msg(x, cid) end
    spawn(fn -> find_quotes(query, answer) end)
    {:ok, max}
  end

  def handle_event({:privmsg, {<<".quote find ", query :: binary>>, _, {clid,_}}}, max) do
    answer = fn(x) -> Mambo.Bot.send_privmsg(x, clid) end
    spawn(fn -> find_quotes(query, answer) end)
    {:ok, max}
  end

  def handle_event({:msg, {<<".quote rm ", num :: binary>>, _, {cid,_,_}}}, max) do
    answer = fn(x) -> Mambo.Bot.send_msg(x, cid) end
    remove_quote(num, answer)
    {:ok, max}
  end

  def handle_event({:privmsg, {<<".quote rm ", num :: binary>>, _, {clid,_}}}, max) do
    answer = fn(x) -> Mambo.Bot.send_privmsg(x, clid) end
    remove_quote(num, answer)
    {:ok, max}
  end

  def handle_event({:msg, {<<".quote ", num :: binary>>, _, {cid,_,_}}}, max) do
    answer = fn(x) -> Mambo.Bot.send_msg(x, cid) end
    get_quote(num, answer)
    {:ok, max}
  end

  def handle_event({:privmsg, {<<".quote ", num :: binary>>, _, {clid,_}}}, max) do
    answer = fn(x) -> Mambo.Bot.send_privmsg(x, clid) end
    get_quote(num, answer)
    {:ok, max}
  end

  def handle_event({:msg, {".quote", _, {cid,_,_}}}, max) do
    answer = fn(x) -> Mambo.Bot.send_msg(x, cid) end
    get_random_quote(answer)
    {:ok, max}
  end

  def handle_event({:privmsg, {".quote", _, {clid,_}}}, max) do
    answer = fn(x) -> Mambo.Bot.send_privmsg(x, clid) end
    get_random_quote(answer)
    {:ok, max}
  end

  def handle_event(_, max) do
    {:ok, max}
  end

  # Helpers

  defp add_quote(id, content) do
    c = String.strip(content)
    if c == "" do
      :error
    else
      {:ok, Mambo.Brain.add_quote(id, c)}
    end
  end

  defp find_quotes(query, answer) do
    c = String.strip(query)
    if c == "" do
      answer.("Huh?")
    else
      case Mambo.Brain.find_quotes(query) do
        [] ->
          answer.("No quotes found matching criteria.")
        [q] ->
          {^q, content} = Mambo.Brain.get_quote(q)
          answer.("[b]Quote #{q}:[/b] #{content}")
        quotes ->
          s = Enum.sort(quotes) |> Enum.join(", ")
          answer.("Quotes found: #{s}.")
      end
    end
  end

  defp remove_quote(num, answer) do
    num = String.strip(num)
    case Integer.parse(num) do
      {id, ""} ->
        Mambo.Brain.remove_quote(id)
        answer.("Quote [b]#{id}[/b] removed.")
      _ ->
        answer.("Invalid quote id [b]#{num}[/b].")
    end
  end

  defp get_quote(num, answer) do
    num = String.strip(num)
    case Integer.parse(num) do
      {id, ""} ->
        case Mambo.Brain.get_quote(id) do
          {^id, content} ->
            answer.("[b]Quote #{id}:[/b] #{content}")
          :not_found ->
            answer.("Quote [b]#{id}[/b] not found.")
        end
      _ ->
        answer.("Invalid quote id [b]#{num}[/b].")
    end
  end

  defp get_random_quote(answer) do
    case Mambo.Brain.get_random_quote() do
      {id, content} ->
        answer.("[b]Quote #{id}:[/b] #{content}")
      :no_quotes ->
        answer.("No quotes found.")
      :not_found ->
        answer.("Quote not found.")
    end
  end
end
