defmodule Mueval do

  use GenEvent.Behaviour

  @muevalcmd "/home/mrshankly/.cabal/bin/mueval-core -XViewPatterns -XTupleSections -XPatternGuards -XArrows -XUnicodeSyntax -XMagicHash -XUnboxedTuples -XTemplateHaskell -mGHC.Base -mGHC.Types -mGHC.Prim -mGHC.Exts -mGHC.ST -mLanguage.Haskell.TH.Syntax -mLanguage.Haskell.TH.Lib -mLanguage.Haskell.TH -e"

  def mueval(exp, callback) do
    res = @muevalcmd <> " \"" <> inspect(exp) <> "\"" |> System.cmd
    "[b]Mueval: [/b]" <> res |> callback.()
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
