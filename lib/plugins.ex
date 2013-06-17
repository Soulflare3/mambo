defmodule Tsmambo.Plugins do
  def start_link(plugins) do
    {:ok, pid} = :gen_event.start_link
    :gen_event.add_handler(pid, Plugins.Manager, [])
    Enum.each(plugins, fn({:on, {plugin, args}}) ->
                           :gen_event.add_handler(pid, plugin, args)
                         (_) ->
                           :ok
                       end)
    {:ok, pid}
  end

  def add_handler(pid, plugin, args) do
    case :gen_event.add_handler(pid, plugin, args) do
      :ok ->
        :ok
      {'EXIT', reason} ->
        {:error, reason}
      other ->
        {:error, other}
    end
  end

  def delete_handler(pid, plugin, args) do
    case :gen_event.delete_handler(pid, plugin, args) do
      :ok ->
        :ok
      {'EXIT', reason} ->
        {:error, reason}
      other ->
        {:error, other}
    end
  end

  def notify(pid, msg) do
    :gen_event.notify(pid, msg)
  end

  def which_handlers(pid) do
    :gen_event.which_handlers(pid)
  end
end
