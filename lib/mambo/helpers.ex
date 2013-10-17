defmodule Mambo.Helpers do
  @moduledoc """
  Contains helper functions used by various Mambo modules.
  """

  @entities HashDict.new [
    {"quot",?"}, {"amp",?&}, {"apos",?'}, {"lt",?<}, {"gt",?>},
    {"nbsp",? }, {"iexcl",?¡}, {"cent",?¢}, {"pound",?£}, {"curren",?¤},
    {"yen",?¥}, {"brvbar",?¦}, {"sect",?§}, {"uml",?¨}, {"copy",?©},
    {"ordf",?ª}, {"laquo",?«}, {"not",?¬}, {"reg",?®}, {"macr",?¯},
    {"deg",?°}, {"plusmn",?±}, {"sup2",?²}, {"sup3",?³}, {"acute",?´},
    {"micro",?µ}, {"para",?¶}, {"middot",?·}, {"cedil",?¸}, {"sup1",?¹},
    {"ordm",?º}, {"raquo",?»}, {"frac14",?¼}, {"frac12",?½}, {"frac34",?¾},
    {"iquest",?¿}, {"Agrave",?À}, {"Aacute",?Á}, {"Acirc",?Â}, {"Atilde",?Ã},
    {"Auml",?Ä}, {"Aring",?Å}, {"AElig",?Æ}, {"Ccedil",?Ç}, {"Egrave",?È},
    {"Eacute",?É}, {"Ecirc",?Ê}, {"Euml",?Ë}, {"Igrave",?Ì}, {"Iacute",?Í},
    {"Icirc",?Î}, {"Iuml",?Ï}, {"ETH",?Ð}, {"Ntilde",?Ñ}, {"Ograve",?Ò},
    {"Oacute",?Ó}, {"Ocirc",?Ô}, {"Otilde",?Õ}, {"Ouml",?Ö}, {"times",?×},
    {"Oslash",?Ø}, {"Ugrave",?Ù}, {"Uacute",?Ú}, {"Ucirc",?Û}, {"Uuml",?Ü},
    {"Yacute",?Ý}, {"THORN",?Þ}, {"szlig",?ß}, {"agrave",?à}, {"aacute",?á},
    {"acirc",?â}, {"atilde",?ã}, {"auml",?ä}, {"aring",?å}, {"aelig",?æ},
    {"ccedil",?ç}, {"egrave",?è}, {"eacute",?é}, {"ecirc",?ê}, {"euml",?ë},
    {"igrave",?ì}, {"iacute",?í}, {"icirc",?î}, {"iuml",?ï}, {"eth",?ð},
    {"ntilde",?ñ}, {"ograve",?ò}, {"oacute",?ó}, {"ocirc",?ô}, {"otilde",?õ},
    {"ouml",?ö}, {"divide",?÷}, {"oslash",?ø}, {"ugrave",?ù}, {"uacute",?ú},
    {"ucirc",?û}, {"uuml",?ü}, {"yacute",?ý}, {"thorn",?þ}, {"yuml",?ÿ},
    {"OElig",?Œ}, {"oelig",?œ}, {"Scaron",?Š}, {"scaron",?š}, {"Yuml",?Ÿ},
    {"fnof",?ƒ}, {"circ",?ˆ}, {"tilde",?˜}, {"Alpha",?Α}, {"Beta",?Β},
    {"Gamma",?Γ}, {"Delta",?Δ}, {"Epsilon",?Ε}, {"Zeta",?Ζ}, {"Eta",?Η},
    {"Theta",?Θ}, {"Iota",?Ι}, {"Kappa",?Κ}, {"Lambda",?Λ}, {"Mu",?Μ},
    {"Nu",?Ν}, {"Xi",?Ξ}, {"Omicron",?Ο}, {"Pi",?Π}, {"Rho",?Ρ},
    {"Sigma",?Σ}, {"Tau",?Τ}, {"Upsilon",?Υ}, {"Phi",?Φ}, {"Chi",?Χ},
    {"Psi",?Ψ}, {"Omega",?Ω}, {"alpha",?α}, {"beta",?β}, {"gamma",?γ},
    {"delta",?δ}, {"epsilon",?ε}, {"zeta",?ζ}, {"eta",?η}, {"theta",?θ},
    {"iota",?ι}, {"kappa",?κ}, {"lambda",?λ}, {"mu",?μ}, {"nu",?ν}, {"xi",?ξ},
    {"omicron",?ο}, {"pi",?π}, {"rho",?ρ}, {"sigmaf",?ς}, {"sigma",?σ},
    {"tau",?τ}, {"upsilon",?υ}, {"phi",?φ}, {"chi",?χ}, {"psi",?ψ},
    {"omega",?ω}, {"thetasym",?ϑ}, {"upsih",?ϒ}, {"piv",?ϖ}, {"ensp",   ? },
    {"emsp",? }, {"thinsp",? }, {"ndash",?–}, {"mdash",?—}, {"lsquo",?‘},
    {"rsquo",?’}, {"sbquo",?‚}, {"ldquo",?“}, {"rdquo",?”}, {"bdquo",?„},
    {"dagger",?†}, {"Dagger",?‡}, {"bull",?•}, {"hellip",?…}, {"permil",?‰},
    {"prime",?′}, {"Prime",?″}, {"lsaquo",?‹}, {"rsaquo",?›}, {"oline",?‾},
    {"frasl",?⁄}, {"euro",?€}, {"image",?ℑ}, {"weierp",?℘}, {"real",?ℜ},
    {"trade",?™}, {"alefsym",?ℵ}, {"larr",?←}, {"uarr",?↑}, {"rarr",?→},
    {"darr",?↓}, {"harr",?↔}, {"crarr",?↵}, {"lArr",?⇐}, {"uArr",?⇑},
    {"rArr",?⇒}, {"dArr",?⇓}, {"hArr",?⇔}, {"forall",?∀}, {"part",?∂},
    {"exist",?∃}, {"empty",?∅}, {"nabla",?∇}, {"isin",?∈}, {"notin",?∉},
    {"ni",?∋}, {"prod",?∏}, {"sum",?∑}, {"minus",?−}, {"lowast",?∗},
    {"radic",?√}, {"prop",?∝}, {"infin",?∞}, {"ang",?∠}, {"and",?∧},
    {"or",?∨}, {"cap",?∩}, {"cup",?∪}, {"int",?∫}, {"there4",?∴},
    {"sim",?∼}, {"cong",?≅}, {"asymp",?≈}, {"ne",?≠}, {"equiv",?≡},
    {"le",?≤}, {"ge",?≥}, {"sub",?⊂}, {"sup",?⊃}, {"nsub",?⊄},
    {"sube",?⊆}, {"supe",?⊇}, {"oplus",?⊕}, {"otimes",?⊗}, {"perp",?⊥},
    {"sdot",?⋅}, {"vellip",?⋮}, {"lceil",?⌈}, {"rceil",?⌉}, {"lfloor",?⌊},
    {"rfloor",?⌋}, {"lang",?〈}, {"rang",?〉}, {"loz",?◊}, {"spades",?♠},
    {"clubs",?♣}, {"hearts",?♥}, {"diams",?♦}
  ]

  @doc """
  Reads the settings file. Returns a `Mambo.Bot.Settings` record.
  """
  @spec get_settings() :: Mambo.Bot.Settings[name: String.t, user: String.t, pass: String.t,
                                             host: String.t, port: integer, bot_id: String.t,
                                             admins: [String.t], scripts: [{atom, [term]}]]
  def get_settings() do
    {:ok, data} = File.read("settings.json")

    {:ok, s} = JSEX.decode(data)
    p = Enum.map(s["scripts"], fn([{_,name}, {_,args}]) ->
                                 {binary_to_atom("Elixir." <> name),args}
                               end)

    Mambo.Bot.Settings[
      name: s["name"],
      user: s["user"],
      pass: s["pass"],
      host: s["host"],
      port: s["port"],
      bot_id: s["bot_id"],
      admins: s["admins"],
      scripts: p
    ]
  end

  @doc """
  Escapes a string to send to the server query.
  """
  @spec escape(String.t) :: String.t
  def escape(s) do
    escape(s, [])
  end

  defp escape("", es), do: Enum.reverse(es) |> String.from_char_list!
  defp escape(<<?\\, r :: binary>>, es), do: escape(r, ["\\\\" | es])
  defp escape(<<?/,  r :: binary>>, es), do: escape(r, ["\\/"  | es])
  defp escape(<<? ,  r :: binary>>, es), do: escape(r, ["\\s"  | es])
  defp escape(<<?|,  r :: binary>>, es), do: escape(r, ["\\p"  | es])
  defp escape(<<?\a, r :: binary>>, es), do: escape(r, ["\\a"  | es])
  defp escape(<<?\b, r :: binary>>, es), do: escape(r, ["\\b"  | es])
  defp escape(<<?\f, r :: binary>>, es), do: escape(r, ["\\f"  | es])
  defp escape(<<?\n, r :: binary>>, es), do: escape(r, ["\\n"  | es])
  defp escape(<<?\r, r :: binary>>, es), do: escape(r, ["\\r"  | es])
  defp escape(<<?\t, r :: binary>>, es), do: escape(r, ["\\t"  | es])
  defp escape(<<?\v, r :: binary>>, es), do: escape(r, ["\\v"  | es])
  defp escape(<<chr :: utf8, r :: binary>>, es), do: escape(r, [chr | es])

  @doc """
  Unescape a string from the server query.
  """
  @spec unescape(String.t) :: String.t
  def unescape(s) do
    unescape(s, [])
  end

  defp unescape("", es), do: Enum.reverse(es) |> String.from_char_list!
  defp unescape(<<"\\\\", r :: binary>>, es), do: unescape(r, [?\\ | es])
  defp unescape(<<"\\/",  r :: binary>>, es), do: unescape(r, [?/  | es])
  defp unescape(<<"\\s",  r :: binary>>, es), do: unescape(r, [?   | es])
  defp unescape(<<"\\p",  r :: binary>>, es), do: unescape(r, [?|  | es])
  defp unescape(<<"\\a",  r :: binary>>, es), do: unescape(r, [?\a | es])
  defp unescape(<<"\\b",  r :: binary>>, es), do: unescape(r, [?\b | es])
  defp unescape(<<"\\f",  r :: binary>>, es), do: unescape(r, [?\f | es])
  defp unescape(<<"\\n",  r :: binary>>, es), do: unescape(r, [?\n | es])
  defp unescape(<<"\\r",  r :: binary>>, es), do: unescape(r, [?\r | es])
  defp unescape(<<"\\t",  r :: binary>>, es), do: unescape(r, [?\t | es])
  defp unescape(<<"\\v",  r :: binary>>, es), do: unescape(r, [?\v | es])
  defp unescape(<<"[URL]", r :: binary>>, es), do: unescape(r, es)
  defp unescape(<<"[\\/URL]", r :: binary>>, es), do: unescape(r, es)
  defp unescape(<<chr :: utf8, r :: binary>>, es), do: unescape(r, [chr | es])

  @doc """
  Converts HTML entities to their respective characters.
  """
  @spec decode_html(String.t) :: String.t
  def decode_html(line) do
    if String.contains?(line, "&") do
      Regex.scan(%r/&(#?[xX]?(?:[0-9a-fA-F]+|\w{1,8}));/, line, return: :index, capture: :first)
      |> r_entities(line, 0, [])
    else
      line
    end
  end

  @doc """
  Format an url string to be displayed on the teamspeak chat.
  """
  @spec format_url(String.t) :: String.t
  def format_url(url) do
    format_url(url, url)
  end

  @spec format_url(String.t, String.t) :: String.t
  def format_url(url, name) do
    "[URL=#{url}]#{name}[/URL]"
  end

  @doc """
  Finds the first url in a string.
  """
  @spec find_url(String.t) :: String.t | nil
  def find_url(line) do
    case Regex.run(%r(http[s]?://[^\s<>"]+|www\.[^\s<>\"]+)i, line) do
      [h | _] -> h
      _ -> nil
    end
  end

  # --------
  # Helpers
  # --------

  defp r_entities([], rest, _, acc) do
    String.from_char_list!(Enum.reverse(acc)) <> rest
  end

  defp r_entities([[{count, len}] | t], <<?&, r :: binary>>, count, acc) do
    {replacement, rest} = get_replacement(len - 2, r)
    r_entities(t, rest, count + len, [replacement | acc])
  end

  defp r_entities(l, <<char :: utf8, rest :: binary>>, count, acc) do
    r_entities(l, rest, count + size(<<char :: utf8>>), [char | acc])
  end

  defp get_replacement(len, s) do
    <<e :: [size(len), binary], ?;, r :: binary>> = s
    case @entities[e] do
    nil ->
      case e do
        <<?#, num :: binary>> ->
          {binary_to_integer(num), r}
        _ ->
          {<<?&, e :: binary, ?;>>, r}
      end
    other ->
      {other, r}
    end
  end
end
