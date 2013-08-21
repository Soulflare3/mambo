defmodule Whatthecommit do
  use GenEvent.Behaviour

  def wtc(callback) do
    url = 'http://whatthecommit.com/index.txt'
    case :httpc.request(:get, {url, []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        callback.("[b]WhatTheCommit:[/b] #{String.strip(body, ?\n)}")
      _ ->
        callback.("Well shit, something went wrong. I blame you.")
    end
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, _userid, :unmuted}, state) do
    case msg do
      ["!wtc"] ->
        callback = fn(x) ->
                       :gen_server.cast(:mambo, {:send_txt, x})
                   end
        spawn(Whatthecommit, :wtc, [callback])
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
