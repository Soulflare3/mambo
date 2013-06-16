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
        str = Regex.replace(%r{\\\\},   line, "\\")
        str = Regex.replace(%r{\\/},    str,  "/")
        str = Regex.replace(%r{\\s},    str,  " ")
        str = Regex.replace(%r{\\p},    str,  "|")
        str = Regex.replace(%r{\\a},    str,  "")
        str = Regex.replace(%r{\\b},    str,  "")
        str = Regex.replace(%r{\\f},    str,  "")
        str = Regex.replace(%r{\\n},    str,  "\n")
        str = Regex.replace(%r{\\r},    str,  "\r")
        str = Regex.replace(%r{\\t},    str,  "    ")
        str = Regex.replace(%r{\\v},    str,  "\n")
        str = Regex.replace(%r{[URL]},  str,  "")
        Regex.replace(%r{[/URL]}, str,  "")
    end
end