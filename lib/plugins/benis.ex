defmodule Benis do
  use GenEvent.Behaviour

  defp benisify(s) do
    Enum.reduce([String.downcase(&1),
      Regex.replace(%r/x/, &1, "cks"),
      Regex.replace(%r/ing/, &1, "in"),
      Regex.replace(%r/you/, &1, "u"),
      Regex.replace(%r/oo/, &1, String.duplicate("u", :random.uniform(5))),
      Regex.replace(%r/ck/, &1, String.duplicate("g", :random.uniform(5))),
      Regex.replace(%r/(t+)(?=[aeiouys]|\b)/, &1, String.duplicate("d", String.length("&") + 1)),
      Regex.replace(%r/p/, &1, "b"),
      Regex.replace(%r/\bthe\b/, &1, "da"),
      Regex.replace(%r/\bc/, &1, "g"),
      Regex.replace(%r/\bis/, &1, "are"),
      Regex.replace(%r/c+(?![eiy])/, &1, String.duplicate("g", :random.uniform(5))),
      Regex.replace(%r/k+(?=[aeiouy]|\b)/, &1, String.duplicate("g", :random.uniform(5))),
      Regex.replace(%r/([?!.]|$)+/, &1, (String.duplicate("&", :random.uniform(5))) <> " " <> ":DD:DDD:D:DD")],
      s,
      fn(f, acc) -> f.(acc) end
    )
  end


  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, _userid}, state) do
    case msg do
      ["!benis", s] ->
        :gen_server.cast(:mambo, {:send_txt, benisify(s)})
        {:ok, state}
       _ ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
