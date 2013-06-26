defmodule Lastfm do
  use GenEvent.Behaviour

  defp parse_song(song) do
    artist = song["artist"]["#text"]
    name = song["name"]
    album = song["album"]["#text"]
    if song["@attr"]["nowplaying"] do
      {artist, name, album, true}
    else
      {artist, name, album}
    end
  end

  defp now_playing(apikey, username, callback) do
    url = 'http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=#{username}&api_key=#{apikey}&format=json'
    case :httpc.request(:get, {url, []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} ->
        case :jsx.decode(body)["recenttracks"]["track"] do
          [] ->
            callback.("[b]Last.fm:[/b] (no result)")
            :dets.close(:ids)
          [h|_t] ->
            case parse_song(h) do
              {artist, name, album, true} ->
                callback.("[b]Last.fm:[/b] [#{username}] is listening to #{artist} - #{name} [#{album}]")
                :dets.close(:ids)
              {artist, name, album} ->
                callback.("[b]Last.fm:[/b] [#{username}] last listened to #{artist} - #{name} [#{album}]")
                :dets.close(:ids)
            end
        end
      _ ->
        callback.("Well shit, something went wrong. I blame you.")
        :dets.close(:ids)
    end
  end

  def init(apikey) do
    {:ok, apikey}
  end

  def handle_event({msg, _user, userid}, apikey) do
    {:ok, :ids} = :dets.open_file(:ids, [{:type, :set}, {:file, 'lastfm.db'}])
    callback = fn(x) ->
                 :gen_server.cast(:mambo, {:send_txt, x})
               end

    case msg do
      ["!np"] ->
        case :dets.lookup(:ids, userid) do
          [{_, username}] ->
            spawn(fn() -> now_playing(apikey, username, callback) end)
            {:ok, apikey}
          [] ->
            callback.("[b]Last.fm:[/b] You're not associated with an username.")
            :dets.close(:ids)
            {:ok, apikey}
        end

      ["!np", username] ->
        spawn(fn() -> now_playing(apikey, username, callback) end)
        {:ok, apikey}

      ["!setuser", username] ->
        :ok = :dets.insert(:ids, {userid, username})
        callback.("[b]Last.fm:[/b] You're now associated with last.fm user [#{username}].")
        :dets.close(:ids)
        {:ok, apikey}

      _ ->
        {:ok, apikey}
    end
  end
end
