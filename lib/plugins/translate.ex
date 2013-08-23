defmodule Translate do
  use GenEvent.Behaviour

  # sl -> from
  # tl -> to
  # example:
  # sl=de and tl=en -> translate from german to english

  defp get_opts(s) do
    case String.split(s, "-") do
      [sl]     -> {:ok, {sl, "en"}}
      [sl, tl] -> {:ok, {sl, tl}}
      _other   -> :error
    end
  end

  def translate(query, sl, tl, callback) do
    q = URI.encode(query)
    url = "https://translate.google.com/translate_a/t?text=#{q}&oe=UTF-8&multires=1&tl=#{tl}&client=p&sl=#{sl}&ie=UTF-8"
    case :httpc.request(:get, {String.to_char_list!(url), []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case :jsx.decode(body)["sentences"] do
          [result | _] ->
            callback.("[b]Translate:[/b] #{result["trans"]}")
          _ ->
            callback.("[b]Translate:[/b] (no result)")
        end
      _ ->
        callback.("Well shit, something went wrong. I blame you.")
    end
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _, _, :unmuted}, state) do
    callback = fn(x) -> :gen_server.cast(:mambo, {:send_txt, x}) end
    case msg do
      ["!tl", m] ->
        spawn(Translate, :translate, [m, "auto", "en", callback])
        {:ok, state}
      [<<"!tl", ?-, opts :: binary>>, m] ->
        case get_opts(opts) do
          {:ok, {sl, tl}} ->
            spawn(Translate, :translate, [m, sl, tl, callback])
            {:ok, state}
          :error ->
            {:ok, state}
        end
      _other ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
