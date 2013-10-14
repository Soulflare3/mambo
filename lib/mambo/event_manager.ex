defmodule Mambo.EventManager do
	@moduledoc """
	Functions used to manage all the Mambo scripts.
	"""

	@manager __MODULE__

	@doc """
	Starts the events manager. Returns `{:ok, pid}` on success.
	"""
	@spec start_link() :: {:ok, pid}
	def start_link() do
		{:ok, _} = :gen_event.start_link({:local, @manager})
	end

	@doc """
	Adds a new event handler to the event manager.
	"""
	@spec install_script(atom, term) :: :ok | {:EXIT, term} | term
	def install_script(script, args) do
		:gen_event.add_handler(@manager, script, args)
	end

	@doc """
	Removes an event handler from the event manager.
	"""
	@spec remove_script(atom, term) :: :ok | {:EXIT, term} | term
	def remove_script(script, args) do
		:gen_event.delete_handler(@manager, script, args)
	end

	@doc """
	Notifies each installed event handler with `msg`. Returns `:ok`.
	"""
	@spec notify(term()) :: :ok
	def notify(msg) do
		:ok = :gen_event.notify(@manager, msg)
	end

	@doc """
	Returns a list of all installed scripts.
	"""
	@spec list_scripts() :: [atom] | [{atom, term}]
	def list_scripts() do
		:gen_event.which_handlers(@manager)
	end
end
