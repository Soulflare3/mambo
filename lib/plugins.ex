defmodule Tsmambo.Plugins do
  def start_link(plugins) do
    {:ok, pid} = :gen_event.start_link
    Enum.each(plugins, fn({plugin, args}) ->
                         :gen_event.add_handler(pid, plugin, args)
                       end)
    {:ok, pid}
  end

  def notify(pid, msg) do
    :gen_event.notify(pid, msg)
  end
end