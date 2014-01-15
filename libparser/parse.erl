-module(parse).
-export([get_symbols/1]).

-record(symbol, {type :: 'refsym' | 'def',
		name :: string(),
		lineno=0 :: integer(),
		line="" :: string()}).

get_symbols(Filename) ->
    Ls = get_lines(Filename),
    Ss = parse_file(Filename),
    lists:map(fun(S) ->
		      L = lists:nth(S#symbol.lineno, Ls),
		      S#symbol{line=binary:bin_to_list(L)}
	      end, Ss).

get_lines(Filename) ->
    {ok, D} = file:read_file(Filename),
    binary:split(D, <<10>>, [global]).

parse_file(Filename) ->
    case epp_dodger:parse_file(Filename) of
	{ok, Forms} ->
	    parse(Forms);
	{error, _Reason} ->
	    []
    end.

parse(Fs) ->
    lists:flatten(lists:map(fun walk_syntax_tree/1, Fs)).

walk_syntax_tree(N) ->
    S = get_symbol(N),
    Subs = lists:map(fun walk_syntax_tree/1, lists:flatten(erl_syntax:subtrees(N))),
    [S | Subs].

check_line_number(N) ->
    LineNumber = erl_syntax:get_pos(N),
    if
	%% LineNumber =:= 0 ->
	%%     1;
	%% LineNumber =:= 0 ->
	%%     throw({invalid_line_number, N});
	true ->
	    LineNumber
    end.

get_symbol(N) ->
    LineNumber = check_line_number(N),
    case erl_syntax:type(N) of
	atom when LineNumber > 0 ->
	    #symbol{type=refsym,
		    name=erl_syntax:atom_literal(N),
		    lineno=LineNumber};
	variable when LineNumber > 0->
	    #symbol{type=refsym,
		    name=erl_syntax:variable_literal(N),
		    lineno=LineNumber};
	function when LineNumber > 0->
	    #symbol{type=def,
		    name=erl_syntax:atom_literal(erl_syntax:function_name(N)),
		    lineno=LineNumber};
	attribute when LineNumber > 0->
	    case erl_syntax:atom_literal(erl_syntax:attribute_name(N)) of
		"define" ->
		    V = find_first_var(N),
		    if
			V =/= [] ->
			    #symbol{type=def,
				    name=erl_syntax:variable_literal(V),
				    lineno=erl_syntax:get_pos(V)};
			true ->
			    []
		    end;
		_ ->
		    []
	    end;
	record_field when LineNumber > 0->
	    #symbol{type=refsym,
		    name=erl_syntax:atom_literal(erl_syntax:record_field_name(N)),
		    lineno=LineNumber};
	_ ->
	    []
	    %% ignored node types:
	    %% application arity_qualifier
	    %% binary binary_field block_expr case_expr 
	    %% catch_expr char class_qualifier clause 
	    %% comment cond_expr conjunction disjunction 
	    %% eof_marker error_marker float form_list 
	    %% fun_expr generator if_expr 
	    %% implicit_fun infix_expr integer list 
	    %% list_comp match_expr module_qualifier 
	    %% nil operator parentheses prefix_expr 
	    %% receive_expr record_access 
	    %% record_expr record_field record_index_expr rule 
	    %% size_qualifier string text try_expr 
	    %% tuple underscore warning_marker
    end.

% Finds the first var node under N
find_first_var(N) ->
    Vs = case erl_syntax:type(N) of
	     variable ->
		 [N];
	     atom ->
		 [];
	     string ->
		 [];
	     _ ->
		 Subs = lists:flatten(erl_syntax:subtrees(N)),
		 lists:flatten(lists:map(fun find_first_var/1, Subs))
	 end,
    if
	length(Vs) > 0 ->
	    hd(Vs);
	true -> []
    end.
