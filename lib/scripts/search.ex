defmodule Search do
	@moduledoc """
	Search google, youtube and images.

	Examples:
	  mambo search <phrase>
	  mambo search youtube <phrase>
	"""

	use GenEvent.Behaviour

	@doc false
	def init(apikey) do
		{:ok, apikey}
	end

	@doc false
	def handle_event({:msg, {"help search", _, _}}, k) do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, k}
	end

	def handle_event({:privmsg, {"help search", _, {id, _}}}, k) do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, k}
	end

	def handle_event({:msg, {msg, _, _}}, k) do
		answer = fn(x) -> Mambo.Bot.send_msg(x) end
		{:ok, re} = Regex.compile("^(?:#{Mambo.Bot.name} )?search(?: (google" <>
		                          "|youtube|yt|img|image(?:s)?)?)? (.*)", "i")

		case Regex.run(re, msg) do
			[_, "", q] ->
				spawn(fn -> search("google", q, k, answer) end)
				{:ok, k}
			[_, e, q] ->
				spawn(fn -> search(e, q, k, answer) end)
				{:ok, k}
			_ ->
				{:ok, k}
		end
	end

	def handle_event({:privmsg, {msg, _, {id, _}}}, k) do
		answer = fn(x) -> Mambo.Bot.send_privmsg(x, id) end
		{:ok, re} = Regex.compile("^(?:#{Mambo.Bot.name} )?search(?: (google" <>
		                          "|youtube|yt|img|image(?:s)?)?)? (.*)", "i")

		case Regex.run(re, msg) do
			[_, "", q] ->
				spawn(fn -> search("google", q, k, answer) end)
				{:ok, k}
			[_, e, q] ->
				spawn(fn -> search(e, q, k, answer) end)
				{:ok, k}
			_ ->
				{:ok, k}
		end
	end

	def handle_event(_, k) do
		{:ok, k}
	end

	# --------
	# Helpers
	# --------

	defp search(<<?i, _ :: binary>>, q, _, answer) do
		url = 'http://ajax.googleapis.com/ajax/services/search/' ++
		      'images?safe=off&v=1.0&q=#{URI.encode(q)}'

		google(url, answer)
	end

	defp search(<<?g, _ :: binary>>, q, _, answer) do
		url = 'https://ajax.googleapis.com/ajax/services/search/' ++
		      'web?safe=off&v=1.0&q=#{URI.encode(q)}'

		google(url, answer)
	end

	defp search(<<?y, _ :: binary>>, q, k, answer) do
		youtube(q, k, answer)
	end

	defp google(url, answer) do
		case :httpc.request(:get, {url, []}, [], body_format: :binary) do
			{:ok, {{_, 200, _}, _, body}} ->
				case :jsx.decode(body)["responseData"]["results"] do
				[] ->
					answer.("No result.")
				[r | _] ->
					answer.("#{Mambo.Helpers.format_url r["unescapedUrl"]}")
				end
			_ ->
				answer.("Something went wrong.")
		end
	end

	defp youtube(q, k, answer) do
		url = "https://www.googleapis.com/youtube/v3/search?" <>
			URI.encode_query(
				[q: q,
				 key: k,
				 part: "id"]) |> String.to_char_list!

		case :httpc.request(:get, {url, []}, [], body_format: :binary) do
		{:ok, {{_, 200, _}, _, body}} ->
			case :jsx.decode(body)["items"] do
				[] ->
					answer.("No result.")
				[v | _] ->
					v_url = "https://www.youtube.com/watch?v=#{v["id"]["videoId"]}"
					answer.("#{Mambo.Helpers.format_url v_url}")
			end
		_ ->
			answer.("Something went wrong.")
		end
	end
end
