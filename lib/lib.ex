defmodule Tsmambo.Lib do

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
    "[URL=#{url}]#{name}[/URL]"
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

  def decode_html(line) do
    if String.contains?(line, "&") do
      Regex.scan(%r/&(#?[xX]?(?:[0-9a-fA-F]+|\w{1,8}));/, line, return: :index, capture: :first)
      |> r_entities(line, 0, [])
    else
      line
    end
  end
end
