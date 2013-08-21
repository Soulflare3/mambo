defmodule Mueval do

  use GenEvent.Behaviour

  @muevalcmd "/home/mrshankly/.cabal/bin/mueval-core --time-limit=3 -XViewPatterns -XTupleSections -XPatternGuards -XArrows -XUnicodeSyntax -XMagicHash -XUnboxedTuples -XTemplateHaskell -mGHC.Base -mGHC.Types -mGHC.Prim -mGHC.Exts -mGHC.ST -mLanguage.Haskell.TH.Syntax -mLanguage.Haskell.TH.Lib -mLanguage.Haskell.TH -e"

  def mueval(exp, callback) do
    res = @muevalcmd <> "\"" <> Inspect.BitString.escape(exp, ?") <> "\"" |> System.cmd
    "[b]Muh eval: [/b]" <> res |> callback.()
  end

  # gen_event callbacks
  def init(_args) do
    {:ok, []}
  end

  def handle_event({["!mueval", exp], _user, _userid, :unmuted}, state) do
    callback = fn(x) -> :gen_server.cast(:mambo, {:send_txt, x}) end
    spawn(Mueval, :mueval, [exp, callback])
    {:ok, state}
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
