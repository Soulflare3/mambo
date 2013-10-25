defmodule Mambo.Brain do
  @moduledoc """
  Implements the Mambo memory using mnesia, 3 tables are available:
    1 - {:mquotes, :number, :quote}
    2 - {:mlastfm, :id, :username}
    3 - {:mscripts, :key, :value}
  """

  # API.

  # Quotes.

  def add_quote(content) do
    number = :mnesia.table_info(:mquotes, :size) + 1
    f = fn() -> :mnesia.write({:mquotes, number, content}) end
    :mnesia.activity(:transaction, f)
  end

  def remove_quote(number) do
    f = fn() -> :mnesia.delete({:mquotes, number}) end
    :mnesia.activity(:transaction, f)
  end

  def get_quote(number) do
    f = fn() ->
      case :mnesia.read({:mquotes, number}) do
        [{:mquotes, ^number, content}] -> content
        [] -> :not_found
      end
    end
    :mnesia.activity(:transaction, f)
  end

  def get_random_quote() do
    case :mnesia.table_info(:mquotes, :size) do
      0 -> :no_quotes
      size ->
        :random.seed(:erlang.now())
        get_quote(:random.uniform(size))
    end
  end

  # Lastfm.

  def add_lastfm_user(id, username) do
    f = fn() -> :mnesia.write({:mlastfm, id, username}) end
    :mnesia.activity(:transaction, f)
  end

  def get_lastfm_user(id) do
    f = fn() ->
      case :mnesia.read({:mlastfm, id}) do
        [{:mlastfm, ^id, username}] -> username
        [] -> :not_found
      end
    end
    :mnesia.activity(:transaction, f)
  end

  def remove_lastfm_user(id) do
    f = fn() -> :mnesia.delete({:mlastfm, id}) end
    :mnesia.activity(:transaction, f)
  end

  # Scripts.

  def put(key, value) do
    f = fn() -> :mnesia.write({:mscripts, key, value}) end
    :mnesia.activity(:transaction, f)
  end

  def get(key) do
    f = fn() ->
      case :mnesia.read({:mscripts, key}) do
        [{:mscripts, ^key, value}] -> value
        [] -> :not_found
      end
    end
    :mnesia.activity(:transaction, f)
  end

  def remove(key) do
    f = fn() -> :mnesia.delete({:mscripts, key}) end
    :mnesia.activity(:transaction, f)
  end
end
