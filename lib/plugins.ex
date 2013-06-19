defmodule Tsmambo.Plugins do
  def start_link(plugins) do
    {:ok, pid} = :gen_event.start_link({:local, :plugins})
    :gen_event.add_handler(pid, Plugins.Manager, [])
    Enum.each(plugins, fn({:on, {plugin, args}}) ->
                           :gen_event.add_handler(pid, plugin, args)
                         (_) ->
                           :ok
                       end)
    {:ok, pid}
  end

  def add_handler(plugin, args) do
    case :gen_event.add_handler(:plugins, plugin, args) do
      :ok ->
        :ok
      {'EXIT', reason} ->
        {:error, reason}
      other ->
        {:error, other}
    end
  end

  def delete_handler(plugin, args) do
    case :gen_event.delete_handler(:plugins, plugin, args) do
      :ok ->
        :ok
      {'EXIT', reason} ->
        {:error, reason}
      other ->
        {:error, other}
    end
  end

  def notify(msg) do
    :gen_event.notify(:plugins, msg)
  end

  def which_handlers() do
    :gen_event.which_handlers(:plugins)
  end
end
