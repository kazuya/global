-module(parse).
-export([parse_file/1]).

-record(symbol, {type :: 'refsym' | 'def',
		name :: string(),
		lineno=0 :: integer(),
		line="" :: string()}).

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

get_symbol(N) ->
    LineNumber = erl_syntax:get_pos(N),
    case erl_syntax:type(N) of
	atom when LineNumber > 0 ->
	    #symbol{type=refsym,
		    name=erl_syntax:atom_literal(N),
		    lineno=LineNumber};
	variable ->
	    #symbol{type=refsym,
		    name=erl_syntax:variable_literal(N),
		    lineno=LineNumber};
	function ->
	    #symbol{type=def,
		    name=erl_syntax:atom_literal(erl_syntax:function_name(N)),
		    lineno=LineNumber};
	attribute ->
	    case erl_syntax:atom_literal(erl_syntax:attribute_name(N)) of
		"define" ->
		    V = find_first_var(N),
		    #symbol{type=def,
			    name=erl_syntax:variable_literal(V),
			    lineno=erl_syntax:get_pos(V)};
		_ ->
		    []
	    end;
	record_field ->
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
