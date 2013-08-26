defmodule Mambo.Brain do
	@moduledoc """
	Implements the Mambo memory, a key-value store that scripts can use.
	"""

	use GenServer.Behaviour

	@brain __MODULE__

	@doc """
	Starts the bot memory process. Returns `{:ok, pid}` on success.
	"""
	@spec start_link() :: {:ok, pid}
	def start_link() do
		{:ok, _} = :gen_server.start_link({:local, @brain}, __MODULE__, [], [])
	end

	@doc """
	Inserts one or more objects into the bot brain. If there already exists an
	object with a key matching the key of some of the given objects, the old
	object will be replaced.
	"""
	@spec set(tuple | [tuple]) :: true
	def set(object) do
		:gen_server.call(@brain, {:set, object})
	end

	@doc """
	Returns a list of all objects with the key `key` in the bot brain.
	"""
	@spec get(term) :: [tuple]
	def get(key) do
		:gen_server.call(@brain, {:get, key})
	end

	@doc """
	Removes all objects from the bot brain with the key `key`.
	"""
	@spec remove(term) :: true
	def remove(key) do
		:gen_server.call(@brain, {:remove, key})
	end

	@doc """
	Dumps the bot brain to the file `filename`.
	"""
	@spec save() :: :ok | {:error, String.t}
	def save() do
		:gen_server.call(@brain, :save)
	end


	# --------------------
	# gen_sever callbacks
	# --------------------

	@doc false
	def init([]) do
		case :ets.file2tab('brain.db', verify: true) do
			{:ok, t} ->
				{:ok, t}
			_ ->
				t = :ets.new(:brain, [:set, :public])
				{:ok, t}
		end
	end

	def handle_call({:set, object}, _, t) do
		r = :ets.insert(t, object)
		{:reply, r, t}
	end

	def handle_call({:get, key}, _, t) do
		r = :ets.lookup(t, key)
		{:reply, r, t}
	end

	def handle_call({:remove, key}, _, t) do
		r = :ets.delete(t, key)
		{:reply, r, t}
	end

	def handle_call(:save, _, t) do
		r = :ets.tab2file(t, 'brain.db')
		{:reply, r, t}
	end

	def handle_call(_, _, t) do
		{:noreply, t}
	end

	@doc false
	def terminate(_, t) do
		:ets.tab2file(t, 'brain.db')
		:ok
	end
end
