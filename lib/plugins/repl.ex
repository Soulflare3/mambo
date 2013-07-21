## DISCLAIMER:
## Experimental plugin, use only if you trust the users that have access
## to your server, expect bugs, please report them.
## UNSAFE! Use with caution, may allow random code to run on your machine.

defmodule Repl do
  use GenEvent.Behaviour

  @allowed_non_local HashDict.new [
    {Bitwise,  :all},
    {Dict,     :all},
    {Enum,     :all},
    {HashDict, :all},
    {Keyword,  :all},
    {List,     :all},
    {ListDict, :all},
    {Regex,    :all},
    {String,   :all},
    {Binary.Chars, [:to_binary]}, # string interpolation
    {Kernel,   [:access]}
  ]

  # with 0 arity
  @restricted_local [:binding, :is_alive, :make_ref, :node, :self]
  @allowed_local [:&&, :.., :<>, :access, :and, :atom_to_binary, :binary_to_atom,
    :case, :cond, :div, :elem, :if, :in, :insert_elem, :is_range, :is_record,
    :is_regex, :match?, :nil?, :or, :rem, :set_elem, :sigil_B, :sigil_C, :sigil_R,
    :sigil_W, :sigil_b, :sigil_c, :sigil_r, :sigil_w, :to_binary, :to_char_list,
    :unless, :xor, :|>, :||, :!, :!=, :!==, :*, :+, :+, :++, :-, :--, :/, :<, :<=,
    :=, :==, :===, :=~, :>, :>=, :abs, :atom_to_binary, :atom_to_list, :binary_part,
    :binary_to_atom, :binary_to_float, :binary_to_integer, :binary_to_integer,
    :binary_to_list, :binary_to_term, :bit_size, :bitstring_to_list, :byte_size,
    :float, :float_to_binary, :float_to_list, :hd, :inspect, :integer_to_binary,
    :integer_to_list, :iolist_size, :iolist_to_binary, :is_atom, :is_binary,
    :is_bitstring, :is_boolean, :is_float, :is_function, :is_integer, :is_list,
    :is_number, :is_tuple, :length, :list_to_atom, :list_to_binary, :list_to_bitstring,
    :list_to_float, :list_to_integer, :list_to_tuple, :max, :min, :not, :round, :size,
    :term_to_binary, :throw, :tl, :trunc, :tuple_size, :tuple_to_list, :fn, :->, :&,
    :__block__, :"{}", :"<<>>", :::, :lc, :inlist, :bc, :inbits, :^, :when, :|]

  defp eval(string, callback) do
    result =
      try do
        do_eval(string)
      rescue
        exception ->
          format_exception exception
      catch
        kind, error ->
          format_error kind, error
      end
    callback.("[b]Elixir:[/b] #{result}")
  end

  defp do_eval(line) do
    case Code.string_to_quoted(line) do
      {:ok, form} ->
        if is_safe? form do
          {value, _} = Code.eval_quoted(form, [], __ENV__)
          inspect value
        else
          raise "restricted"
        end
      {:error, {line, error, token}} ->
        :elixir_errors.parse_error(line, "iex", error, token)
    end
  end

  defp is_safe?({{:., _, [module, fun]}, _, args}) do
    module = Macro.expand(module, __ENV__)
    case HashDict.get(@allowed_non_local, module) do
      :all ->
        is_safe?(args)
      lst when is_list(lst) ->
        (fun in lst) and is_safe?(args)
      _ ->
        false
    end
  end

  # check calls to anonymous functions, eg. f.()
  defp is_safe?({{:., _, f_args}, _, args}) do
    is_safe?(f_args) and is_safe?(args)
  end

  # used with :fn
  defp is_safe?([do: args]) do
    is_safe?(args)
  end

  # used with :'->'
  defp is_safe?({left, _, right}) when is_list(left) do
    is_safe?(left) and is_safe?(right)
  end

  # limit range size
  defp is_safe?({:.., _, [begin, last]}) do
    (last - begin) <= 100 and last < 1000
  end

  # don't size and unit in :::
  defp is_safe?({:::, _, [_, opts]}) do
    do_opts(opts)
  end

  # check 0 arity local functions
  defp is_safe?({dot, _, nil}) when is_atom(dot) do
    not dot in @restricted_local
  end

  defp is_safe?({dot, _, args}) when args != nil do
    (dot in @allowed_local) and is_safe?(args)
  end

  defp is_safe?(lst) when is_list(lst) do
    if length(lst) <= 100 do
      Enum.all?(lst, fn(x) -> is_safe?(x) end)
    else
      false
    end
  end

  defp is_safe?(_) do
    true
  end

  defp do_opts(opt) when is_tuple(opt) do
    case opt do
      {:size, _, _} -> false
      {:unit, _, _} -> false
      _ -> true
    end
  end

  defp do_opts([h|t]) do
    case h do
      {:size, _, _} -> false
      {:unit, _, _} -> false
      _ -> do_opts(t)
    end
  end

  defp do_opts([]), do: true

  defp format_exception(exception) do
    "** (#{inspect exception.__record__(:name)}) #{exception.message}"
  end

  defp format_error(kind, reason) do
    "** (#{kind}) #{inspect(reason)}"
  end

  ## gen_event callbacks

  def init(_args) do
    {:ok, []}
  end

  def handle_event({msg, _user, _userid, :unmuted}, state) do
    case msg do
      ["!elixir", s] ->
        callback = fn(x) ->
                       :gen_server.cast(:mambo, {:send_txt, x})
                   end
        spawn(fn() -> eval(s, callback) end)
        {:ok, state}
      _ ->
        {:ok, state}
    end
  end

  def handle_event(_other, state) do
    {:ok, state}
  end
end
