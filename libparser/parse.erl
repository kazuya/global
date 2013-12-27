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
    lists:map(fun walk_syntax_tree/1, Fs).

walk_syntax_tree(N) ->
    io:format("WST:~n ~s~n", [erl_prettypr:format(N)]),
%    io:format("WST RAW:~n ~s~n", [erl_prettypr:format(N)]),
    io:format("WST RAW:~n ~w~n", [N]),
    case N of 
	{tree, function, _Attr, {func, {tree, atom, {attr, LineNumber, _, _}, Fname}, Rest}}  ->
	    S = #symbol{type=def, name=atom_to_list(Fname), lineno=LineNumber},
	    io:format("## ~w~n", [S]),
	    Subs = lists:map(fun walk_syntax_tree/1, Rest),
	    [S|Subs];
	{tree, clause, _Attr, {clause, Vars, _, Rest}} ->
	    io:format("%% ~w~n", [Rest]),
	    lists:map(fun walk_syntax_tree/1, Vars)
		++ lists:map(fun walk_syntax_tree/1, Rest);
	{tree, application, _Attr, {application, {atom, LineNumber, Fname}, Vars}} ->
	    io:format("$$ ~w: ~w~n", [Fname, Vars]),
	    S = #symbol{type=refsym, name=atom_to_list(Fname), lineno=LineNumber},
	    Subs = lists:map(fun walk_syntax_tree/1, Vars),
	    [S|Subs];
	{tree, infix_expr, _Attr, {infix_expr, _Op, T1, T2}} ->
	    lists:map(fun walk_syntax_tree/1, [T1])
		++ lists:map(fun walk_syntax_tree/1, [T2]);
	{tree, match_expr, _Attr,
	 {match_expr, LeftNode, RightNode}} ->
	    Left = walk_syntax_tree(LeftNode),
	    Right = walk_syntax_tree(RightNode),
	    Left ++ Right;
	{var, LineNumber, VarName} ->
	    [#symbol{type=refsym, name=VarName, lineno=LineNumber}];
	{tree, atom, _Attr, Aname} ->
	    [#symbol{type=refsym, name=Aname}];
	record_expr ->
	    [];
	record_access ->
	    [];
	_ ->
	    []
    end.
