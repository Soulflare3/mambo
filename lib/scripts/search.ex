defmodule Search do
	@moduledoc """
	Search google, youtube and images.

	Examples:
	  .google <phrase>
	  .youtube <phrase>
	  .images <phrase>
	"""

	use GenEvent.Behaviour

	@doc false
	def init(apikey) do
		{:ok, apikey}
	end

	@doc false
	def handle_event({:msg, {<<".help ", cmd :: binary>>, _, _}}, k)
		when cmd in ["google", "yt", "youtube", "img", "image", "images"] do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, k}
	end

	@doc false
	def handle_event({:privmsg, {<<".help ", cmd :: binary>>, _, {id, _}}}, k)
		when cmd in ["google", "yt", "youtube", "img", "image", "images"] do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, k}
	end

	@doc false
	def handle_event({:msg, {msg, _, _}}, k) do
		answer = fn(x) -> Mambo.Bot.send_msg(x) end
		case Regex.run(%r/^(\.g|\.google|\.youtube|\.yt|\.img|\.image(?:s)?) (.*)/i, msg) do
			[_, cmd, q] ->
				spawn(fn -> search(cmd, q, k, answer) end)
				{:ok, k}
			_ ->
				{:ok, k}
		end
	end

	@doc false
	def handle_event({:privmsg, {msg, _, {id, _}}}, k) do
		answer = fn(x) -> Mambo.Bot.send_privmsg(x, id) end
		case Regex.run(%r/^(\.g|\.google|\.youtube|\.yt|\.img|\.image(?:s)?) (.*)/i, msg) do
			[_, cmd, q] ->
				spawn(fn -> search(cmd, q, k, answer) end)
				{:ok, k}
			_ ->
				{:ok, k}
		end
	end

	@doc false
	def handle_event(_, k) do
		{:ok, k}
	end

	# --------
	# Helpers
	# --------

	defp search(<<?., ?i, _ :: binary>>, q, _, answer) do
		url = 'http://ajax.googleapis.com/ajax/services/search/' ++
		      'images?safe=off&v=1.0&q=#{URI.encode(q)}'

		google(url, answer)
	end

	defp search(<<?., ?g, _ :: binary>>, q, _, answer) do
		url = 'https://ajax.googleapis.com/ajax/services/search/' ++
		      'web?safe=off&v=1.0&q=#{URI.encode(q)}'

		google(url, answer)
	end

	defp search(<<?., ?y, _ :: binary>>, q, k, answer) do
		youtube(q, k, answer)
	end

	defp google(url, answer) do
		case :httpc.request(:get, {url, []}, [], body_format: :binary) do
			{:ok, {{_, 200, _}, _, body}} ->
				{:ok, data}  = JSEX.decode(body)
				rdata = data["responseData"]

				case rdata["results"] do
					[] ->
						answer.("No result.")
					[r | _] ->
						result = r["unescapedUrl"]
						spawn(Title, :get_title, [result])
						answer.("#{Mambo.Helpers.format_url result}")
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
			{:ok, data} = JSEX.decode(body)
			case data["items"] do
				[] ->
					answer.("No result.")
				[v | _] ->
					id = v["id"]
					v_url = "https://www.youtube.com/watch?v=#{id["videoId"]}"
					spawn(Title, :get_title, [v_url])
					answer.("#{Mambo.Helpers.format_url v_url}")
			end
		_ ->
			answer.("Something went wrong.")
		end
	end
end
