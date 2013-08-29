defmodule Help do
	@moduledoc """
	Shows mambo help message.

	Examples:
	  help
	"""

	use GenEvent.Behaviour

	@helpmsg """

	Mambo is an extensible irc-style teamspeak 3 bot.
	See 'help <option>' for more information on a specific script.

	Options:
	  ask                 - ask mambo anything
	  benis               - benisify a sentence (*)
	  brainfuck           - brainfuck interpreter
	  cannedreplies       - reply to certain keywords (*)
	  chat | speak | talk - start a private chat with the bot (*)
	  8ball               - ask the magic 8 ball
	  elixir              - interactive elixir
	  gay | rainbow       - rainbow color a phrase (*)
	  lastfm              - shows the last (or current) played song in last.fm (*)
	  math                - calculator
	  random              - luck games
	  search              - search google, youtube and images
	  sux                 - curse something (*)
	  title               - print url title (*)
	  translate           - translate an expression
	  utils               - utility commands
	  wtc                 - prints a commit from whatthecommit.com

	  Options marked with (*) don't work on a private chat.
	"""

	@doc false
	def init([]) do
		{:ok, []}
	end

	@doc false
	def handle_event({:msg, {"help", _, _}}, []) do
		Mambo.Bot.send_msg(@helpmsg)
		{:ok, []}
	end

	@doc false
	def handle_event({:privmsg, {"help", _, {id, _}}}, []) do
		Mambo.Bot.send_privmsg(@helpmsg, id)
		{:ok, []}
	end

	@doc false
	def handle_event(_, []) do
		{:ok, []}
	end
end
