defmodule Lastfm do
  @moduledoc """
  Shows the last (or current) played song in last.fm.

  Examples:
    .np
    .np <last.fm user>
    .np set <last.fm user>
  """

  use GenEvent.Behaviour

  @doc false
  def init(apikey) do
    {:ok, apikey}
  end

  @doc false
  def handle_event({:msg, {".help np", _, {cid,_,_}}}, k) do
    Mambo.Bot.send_msg(<<?\n, @moduledoc>>, cid)
    {:ok, k}
  end

  @doc false
  def handle_event({:privmsg, {".help np", _, {clid,_}}}, k) do
    Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, clid)
    {:ok, k}
  end

  @doc false
  def handle_event({:msg, {".np", _, {cid,_,id}}}, k) do
    spawn(fn -> get_song(Mambo.Brain.get_lastfm_user(id), k, cid) end)
    {:ok, k}
  end

  @doc false
  def handle_event({:msg, {<<".np set ", u :: binary>>, _, {cid,_,id}}}, k) do
    :ok = Mambo.Brain.add_lastfm_user(id, u)
    Mambo.Bot.send_msg("You're now associated with last.fm user [b]#{u}[/b].", cid)
    {:ok, k}
  end

  @doc false
  def handle_event({:msg, {<<".np ", u :: binary>>, _, {cid,_,_}}}, k) do
    spawn(fn -> get_song(u, k, cid) end)
    {:ok, k}
  end

  @doc false
  def handle_event(_, k) do
    {:ok, k}
  end

  # --------
  # Helpers
  # --------

  defp get_song(:not_found, _, cid) do
    Mambo.Bot.send_msg("There is no last.fm username associated with your identity.", cid)
  end

  defp get_song(u, k, cid) do
    url = "http://ws.audioscrobbler.com/2.0/?" <>
      URI.encode_query(
        [method: "user.getrecenttracks",
         user: u,
         api_key: k,
         format: "json"]) |> String.to_char_list!

    case :httpc.request(:get, {url, []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        {:ok, song} = JSEX.decode(body)
        case song["recenttracks"] do
          nil ->
            Mambo.Bot.send_msg("No result.", cid)
          t ->
            case t["track"] do
              nil ->
                Mambo.Bot.send_msg("No result.", cid)
              [s | _] ->
                name   = s["name"]
                artist = s["artist"]
                Mambo.Bot.send_msg("[b]#{name}[/b] by [b]#{artist["#text"]}[/b].", cid)
            end
        end
      _ ->
        Mambo.Bot.send_msg("Something went wrong.", cid)
    end
  end
end
