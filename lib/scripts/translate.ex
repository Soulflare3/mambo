defmodule Translate do
	@moduledoc """
	Mambo is a polyglot, he will translate anything for you.

	Examples:
	  .translate <phrase>
	  .translate <input language> <target language> <phrase>
	"""

	use GenEvent.Behaviour

	@languages [
		{"af", "afrikaans"},
		{"sq", "albanian"},
		{"ar", "arabic"},
		{"az", "azerbaijani"},
		{"eu", "basque"},
		{"bn", "bengali"},
		{"be", "belarusian"},
		{"bg", "bulgarian"},
		{"ca", "catalan"},
		{"zh-CN", "chinese"},
		{"zh-CN", "simplified chinese"},
		{"zh-TW", "traditional chinese"},
		{"hr", "croatian"},
		{"cs", "czech"},
		{"da", "danish"},
		{"nl", "dutch"},
		{"en", "english"},
		{"eo", "esperanto"},
		{"et", "estonian"},
		{"tl", "filipino"},
		{"fi", "finnish"},
		{"fr", "french"},
		{"gl", "galician"},
		{"ka", "georgian"},
		{"de", "german"},
		{"el", "greek"},
		{"gu", "gujarati"},
		{"ht", "haitian creole"},
		{"iw", "hebrew"},
		{"hi", "hindi"},
		{"hu", "hungarian"},
		{"is", "icelandic"},
		{"id", "indonesian"},
		{"ga", "irish"},
		{"it", "italian"},
		{"ja", "japanese"},
		{"kn", "kannada"},
		{"ko", "korean"},
		{"la", "latin"},
		{"lv", "latvian"},
		{"lt", "lithuanian"},
		{"mk", "macedonian"},
		{"ms", "malay"},
		{"mt", "maltese"},
		{"no", "norwegian"},
		{"fa", "persian"},
		{"pl", "polish"},
		{"pt", "portuguese"},
		{"ro", "romanian"},
		{"ru", "russian"},
		{"sr", "serbian"},
		{"sk", "slovak"},
		{"sl", "slovenian"},
		{"es", "spanish"},
		{"sw", "swahili"},
		{"sv", "swedish"},
		{"ta", "tamil"},
		{"te", "telugu"},
		{"th", "thai"},
		{"tr", "turkish"},
		{"uk", "ukrainian"},
		{"ur", "urdu"},
		{"vi", "vietnamese"},
		{"cy", "welsh"},
		{"yi", "yiddish"}
	]

	# --------------------
	# gen_event callbacks
	# --------------------

	@doc false
	def init([]) do
		langs = Enum.reduce(@languages, [], fn({k,v}, acc) -> [k, v | acc] end)
		|> Enum.join("|")

		{:ok, langs}
	end

	@doc false
	def handle_event({:msg, {<<".help ", cmd :: binary>>, _, _}}, langs)
		when cmd in ["tl", "translate"] do
		Mambo.Bot.send_msg(<<?\n, @moduledoc>>)
		{:ok, langs}
	end

	@doc false
	def handle_event({:privmsg, {<<".help ", cmd :: binary>>, _, {id, _}}}, langs)
		when cmd in ["tl", "translate"] do
		Mambo.Bot.send_privmsg(<<?\n, @moduledoc>>, id)
		{:ok, langs}
	end

	@doc false
	def handle_event({:msg, {msg, _, _}}, langs) do
		answer = fn(x) -> Mambo.Bot.send_msg(x) end

		case Regex.run(%r/^(\.tl|\.translate)(?: (#{langs}))?(?: (#{langs}))? (.*)/, msg) do
			[_, _, "", "", exp] ->
				spawn(fn -> translate("auto", "en", exp, answer) end)
				{:ok, langs}

			[_, _, sl, tl, exp] when sl != "" and tl != "" ->
				spawn(fn -> translate(get_code(sl), get_code(tl), exp, answer) end)
				{:ok, langs}

			_ ->
				{:ok, langs}
		end
	end

	@doc false
	def handle_event({:privmsg, {msg, _, {id, _}}}, langs) do
		answer = fn(x) -> Mambo.Bot.send_privmsg(x, id) end

		case Regex.run(%r/^(\.tl|\.translate)(?: (#{langs}))?(?: (#{langs}))? (.*)/, msg) do
			[_, _, "", "", exp] ->
				spawn(fn -> translate("auto", "en", exp, answer) end)
				{:ok, langs}

			[_, _, sl, tl, exp] when sl != "" and tl != "" ->
				spawn(fn -> translate(get_code(sl), get_code(tl), exp, answer) end)
				{:ok, langs}

			_ ->
				{:ok, langs}
		end
	end

	@doc false
	def handle_event(_, langs) do
		{:ok, langs}
	end

	# --------
	# Helpers
	# --------

	defp get_code(lang) do
		unless ListDict.has_key?(@languages, lang) do
			Enum.reduce(@languages, "en", fn({k,v}, acc) ->
				if String.downcase(v) == String.downcase(lang), do: k, else: acc
			end)
		else
			lang
		end
	end

	defp translate(sl, tl, exp, answer) do
		url = "https://translate.google.com/translate_a/t?" <>
			URI.encode_query(
				[client: "p",
				 oe: "UTF-8",
				 ie: "UTF-8",
				 hl: "en",
				 multires: 1,
				 sc: 1,
				 sl: sl,
				 ssel: 0,
				 tl: tl,
				 tsel: 0,
				 uptl: "en",
				 text: exp]) |> String.to_char_list!

		case :httpc.request(:get, {url, []}, [], body_format: :binary) do
			{:ok, {{_, 200, _}, _, body}} ->
				{:ok, data} = JSEX.decode(body)
				ilang = @languages[data["src"]]
				tlang = @languages[tl]
				sentences = hd(data["sentences"])
				trans = sentences["trans"]

				if sl == "auto" do
					answer.("[b]#{exp}[/b] is #{ilang} for [b]#{trans}[/b].")
				else
					answer.("The #{ilang} [b]#{exp}[/b] translates as [b]#{trans}[/b] in #{tlang}.")
				end
			_ ->
				answer.("Something went wrong.")
		end
	end
end
