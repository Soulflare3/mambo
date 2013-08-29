defmodule Lastfm do
	@moduledoc """
	Shows the last (or current) played song in last.fm.

	Examples:
	  !np
	  !np <last.fm user>
	  !np set <last.fm user>
	"""

	use GenEvent.Behaviour

	@doc false
	def init(apikey) do
		{:ok, apikey}
	end

	@doc false
	def handle_event({:msg, {"help lastfm", _, _}}, k) do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, k}
	end

	@doc false
	def handle_event({:privmsg, {"help lastfm", _, {id, _}}}, k) do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, k}
	end

	@doc false
	def handle_event({:msg, {"!np", _, {_, id}}}, k) do
		spawn(fn -> get_song(Mambo.Brain.get(id), k) end)
		{:ok, k}
	end

	@doc false
	def handle_event({:msg, {<<"!np set ", u :: binary>>, _, {_, id}}}, k) do
		true = Mambo.Brain.set({id, u})
		:ok = Mambo.Brain.save
		Mambo.Bot.send_msg("You're now associated with last.fm user [b]#{u}[/b].")
		{:ok, k}
	end

	@doc false
	def handle_event({:msg, {<<"!np ", u :: binary>>, _, _}}, k) do
		spawn(fn -> get_song(u, k) end)
		{:ok, k}
	end

	@doc false
	def handle_event(_, k) do
		{:ok, k}
	end

	# --------
	# Helpers
	# --------

	defp get_song(nil, _) do
		Mambo.Bot.send_msg("There is no last.fm username associated with your identity.")
	end

	defp get_song(u, k) do
		url = "http://ws.audioscrobbler.com/2.0/?" <>
			URI.encode_query(
				[method: "user.getrecenttracks",
				 user: u,
				 api_key: k,
				 format: "json"]) |> String.to_char_list!

		case :httpc.request(:get, {url, []}, [], body_format: :binary) do
			{:ok, {{_, 200, _}, _, body}} ->
				song = :jsx.decode(body)
				case ListDict.get(song, "recenttracks") do
					nil ->
						Mambo.Bot.send_msg("No result.")
					t ->
						case ListDict.get(t, "track") do
							nil ->
								Mambo.Bot.send_msg("No result.")
							[s | _] ->
								Mambo.Bot.send_msg("[b]#{s["name"]}[/b] by [b]#{s["artist"]["#text"]}[/b].")
						end
				end
			_ ->
				Mambo.Bot.send_msg("Something went wrong.")
		end
	end
end
