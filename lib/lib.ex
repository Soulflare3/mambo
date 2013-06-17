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
        "[URL]#{url}[/URL]"
    end

    def find_url(line) do
        Regex.run(%r{http[s]?://[^\s<>"]+|www\.[^\s<>\"]+}iu, line)
    end

    def encode(line) do
        str = Regex.replace(%r{\\}, line, %B{\\\\})
        str = Regex.replace(%r{/}, str, %B{\\/})
        str = Regex.replace(%r{ }, str, %B{\\s})
        str = Regex.replace(%r{\|}, str, %B{\\p})
        str = Regex.replace(%r{\n}, str, %B{\\n})
        str = Regex.replace(%r{\r}, str, %B{\\r})
        Regex.replace(%r{   }, str, %B{\\t})
    end

    def decode(line) do
        String.replace(line, %b{\\\\}, %b{\\})
        |> String.replace(%b{\\/},    %b{/})
        |> String.replace(%b{\\s},    %b{ })
        |> String.replace(%b{\\p},    %b{|})
        |> String.replace(%b{\\a},    "")
        |> String.replace(%b{\\b},    "")
        |> String.replace(%b{\\f},    "")
        |> String.replace(%b{\\n},    %b{\n})
        |> String.replace(%b{\\r},    %b{\r})
        |> String.replace(%b{\\t},    %b{    })
        |> String.replace(%b{\\v},    %b{\n})
        |> String.replace(%b{[URL]},  "")
        |> String.replace(%b{[/URL]}, "")
    end
end