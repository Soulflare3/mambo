defmodule Tsmambo.Lib do

  #Not very safe, change it!
  def consult(file) do
    try do
      {:ok, bin} = File.read(file)
      {term, _} = Code.eval_string(bin)
      {:ok, term}
    rescue
      error ->
        {:error, "#{error.message}"}
    end
  end

  def format_url(url) do
    format_url(url, url)
  end

  def format_url(url, name) do
    "[URL=#{url} ]#{name}[/URL]"
  end

  def find_url(line) do
    Regex.run(%r(http[s]?://[^\s<>"]+|www\.[^\s<>\"]+)iu, line)
  end

  def encode(line) do
    String.replace(line, %b(\\), %b(\\\\))
    |> String.replace(%b(/),   %b(\\/))
    |> String.replace(%b( ),   %b(\\s))
    |> String.replace(%b(|),   %b(\\p))
    |> String.replace(%b(\n),  %b(\\n))
    |> String.replace(%b(\r),  %b(\\r))
    |> String.replace(%b(   ), %b(\\t))
  end

  def decode(line) do
    String.replace(line, %b(\\\\), %b(\\))
    |> String.replace(%b(\\/),    %b(/))
    |> String.replace(%b(\\s),    %b( ))
    |> String.replace(%b(\\p),    %b(|))
    |> String.replace(%b(\\a),    "")
    |> String.replace(%b(\\b),    "")
    |> String.replace(%b(\\f),    "")
    |> String.replace(%b(\\n),    %b(\n))
    |> String.replace(%b(\\r),    %b(\r))
    |> String.replace(%b(\\t),    %b(    ))
    |> String.replace(%b(\\v),    %b(\n))
    |> String.replace(%b([URL]),  "")
    |> String.replace(%b([URL=),  "")
    |> String.replace(%b([/URL]), "")
  end
end