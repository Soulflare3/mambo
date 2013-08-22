# Tsmambo

Tsmambo is an extensible irc-style teamspeak3 bot. It was used as a learning project for elixir.
Pull requests or any other kind of help in making the bot better is highly appreciated.

Getting started
---------------

You will need to create a `settings.cfg` file, use the `settings.cfg.sample` as a guide.
Username and password are the teamspeak3 server query credentials, name is the nickname you want for the bot.
On the plugin list, place a list of all the plugins you plan to use in the following form,
`{:on, {ModName, [Args]}}`, the first element makes the plugin load when the bot is started, if you don't that just change this value to `:off`.

When you're happy with your `settings.cfg` just do the following to compile and start the bot:

	mix deps.get
	make
	make run

The bot will only work on the default channel, I will probably add the ability to choose the bot
channel on the settings file, for the time being if you want it in another channel you will have to move it
manually in your teamspeak3 client. If you want to keep the bot running on your server start it in a screen or
tmux session.

Plugins
-------

Check the [wiki page](https://github.com/mrshankly/tsmambo/wiki/Plugins).

License
-------

All files under this repository fall under the MIT License. Check LICENSE for more details.
