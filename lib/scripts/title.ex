defmodule Title do
	@moduledoc """
	Print the title of a webpage everytime its url is written on the chat.

	Examples:
	  http://www.erlang.org/
	  http://elixir-lang.org/
	"""

	use GenEvent.Behaviour

	@doc false
	def init([]) do
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"help title", _, _}}, []) do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"help title", _, {id, _}}}, []) do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {msg, _, _}}, []) do
		case Mambo.Helpers.find_url(msg) do
			nil ->
				{:ok, []}
			url ->
				spawn(fn -> get_title(url) end)
				{:ok, []}
		end
	end

	@doc false
	def handle_event(_, []) do
		{:ok, []}
	end

	@doc """
	Sends the title of the url `url` to the channel the bot is operating.
	"""
	def get_title(url) do
		headers = [{'User-Agent', 'Mozilla/5.0'},
		           {'Cookie', 'locale=en_US; path=/; domain=.facebook.com'}]

		{:ok, ref} = :httpc.request(:get, {String.to_char_list!(url), headers}, [],
			                        sync: false, stream: :self, body_format: :binary)

		receive_chunk(ref, "", 5000)
	end

	# --------
	# Helpers
	# --------

	defp receive_chunk(_, body, len) when len <= 0 do
		case Regex.run(%r\<title[^>]*>([^<]+)</title>\im, body, capture: [1]) do
			nil ->
				:ok
			[title] ->
				title = String.strip(title) |> String.split("\n") |> Enum.join
				        |> Mambo.Helpers.decode_html

				Mambo.Bot.send_msg(<<"[b]Title:[/b] ", title :: binary>>)
		end
	end

	defp receive_chunk(ref, body, len) do
		receive do
			{:http, {^ref, :stream_start, headers}} ->
				content_type = String.from_char_list!(headers['content-type'])
				               |> String.split(";") |> hd

				if content_type == "text/html" do
					receive_chunk(ref, body, len)
				else
					Mambo.Bot.send_msg(<<"[b]Content Type:[/b] ", content_type :: binary>>)
				end

			{:http, {^ref, :stream, data}} ->
				receive_chunk(ref, body <> data, len - size(data))

			{:http, {^ref, :stream_ent, _}} ->
				receive_chunk(ref, body, 0)

		after
			5000 ->
				:ok
		end
	end
end
