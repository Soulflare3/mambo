## DISCLAIMER:
## Experimental plugin, use only if you trust the users that have access
## to your server, expect bugs, please report them.
## UNSAFE! Use with caution, may allow random code to run on your machine.

defmodule Repl do
  use GenEvent.Behaviour

  @allowed_mods [Bitwise, Dict, Enum, HashDict, Keyword, List, ListDict,
                 Regex, String, URI, :dict, :lists, :math, :orddict, :proplists,
                 :re, :sets, :string]

  @allowed_funs [:fn, :'->', :&, :=, :==, :===, :>=, :<=, :!=, :!==, :>,
                 :<, :and, :or, :||, :&&, :!, :*, :+, :-, :/, :++, :--, :<>,
                 :is_atom, :is_binary, :is_bitstring, :is_boolean, :is_float,
                 :is_function, :is_integer, :is_list, :is_number, :is_pid,
                 :is_port, :is_record, :is_reference, :is_tuple, :is_exception,
                 :abs, :bit_size, :byte_size, :div, :elem, :float, :hd, :length,
                 :rem, :round, :size, :tl, :trunc, :tuple_size, :lc, :inlist,
                 :__block__, :atom_to_binary, :atom_to_list, :binary_part,
                 :binary_to_atom, :binary_to_float, :binary_to_integer,
                 :binary_to_list, :binary_to_term, :bitstring_to_list,
                 :float_to_binary, :float_to_list, :integer_to_binary,
                 :integer_to_list, :iolist_size, :iolist_to_binary,
                 :list_to_atom, :list_to_binary, :list_to_bitstring,
                 :list_to_float, :list_to_integer, :list_to_tuple, :max, :min,
                 :term_to_binary, :.., :=~, :__B__, :__C__, :__R__, :__W__,
                 :__b__, :__c__, :__r__, :__w__, :delete_elem, :in, :is_range,
                 :is_regex, :match?, :nil?, :set_elem, :to_binary, :to_char_list,
                 :xor, :'|>', :access, :'{}', :'<<>>', :::]

  defp eval(string, callback) do
    case Code.string_to_ast(string) do
      {:ok, ast} ->
        try do
          {value, _} = Code.eval_quoted(safe(ast, __ENV__))
          callback.("[b]Elixir:[/b] #{inspect value}")
        catch
          reason ->
            callback.("[b]Elixir:[/b] #{reason}")
        rescue
          error ->
            callback.("[b]Elixir:[/b] #{error.message}")
        end
      {:error, {line, desc, info}} ->
        callback.("[b]Elixir:[/b] #{line}: #{desc}#{info}")
    end
  end

  #check modules
  defp safe({{:., _, [module, fun]} = dot, meta, args} = tree, caller) do
    module = Macro.expand(module, caller)
    if is_atom(module) do
      cond do
        module == Kernel and fun == :access ->
          {dot, meta, safe(args, caller)}
        module in @allowed_mods ->
          {dot, meta, safe(args, caller)}
        true ->
          throw "#{inspect module} is not allowed."
      end
    else
      throw "Invalid call expression #{inspect Macro.to_binary(tree)}"
    end
  end

  #check calls to anonymous functions, eg. f.()
  defp safe({{:., f_meta, f_args}, meta, args}, caller) do
    {{:., f_meta, safe(f_args, caller)}, meta, safe(args, caller)}
  end

  #check functions
  defp safe({dot, meta, args}, caller) when args != nil do
    if dot in @allowed_funs do
      {dot, meta, safe(args, caller)}
    else
      throw "#{inspect dot} is not allowed."
    end
  end

  #used with :'->'
  defp safe({lst, other}, caller) when is_list(lst) do
    {lst, safe(other, caller)}
  end

  #used with :fn
  defp safe([do: other], caller) do
    [do: safe(other, caller)]
  end

  defp safe(other, caller) when is_list(other) do
    Enum.map(other, fn(x) -> safe(x, caller) end)
  end

  defp safe(other, _caller) do
    other
  end

  ## gen_event callbacks

  def init(_args) do
    {:ok, []}
  end

  def handle_event({gen_server, msg, _user, _userid}, state) do
    case msg do
      ["!elixir", s] ->
        callback = fn(x) ->
                       :gen_server.cast(gen_server, {:send_txt, x})
                   end
        spawn(fn() -> eval(s, callback) end)
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end
end
