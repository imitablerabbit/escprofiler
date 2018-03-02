%% @doc
%% escprofile is a simple escript which profiles the time it takes for each
%% function call in another escript file.
%%
%% This script works by converting the escript source code into the erlang
%% abstract form and wraps function expressions in the profiling function
%% abstract. The new wrapped abstract form is then evaluated and the data
%% is printed out.
%% @end

-module(escprofile).
-author(markhillman).
-export([main/1]).

main([Filename]) ->
	% Find the other escript file
	%{ok, Bin} = file:read_file(Filename), % read all of the file
	%Filedata = binary_to_list(Bin),

	% Lex and parse the code
	%{ok, Tokens, _} = erl_scan:string(Filedata),
	{ok, Form} = epp:parse_file(Filename, []),
	io:format("ok", []),
	%{ok, Abs} = erl_parse:parse(Tokens),
	io:format("ok", []),

	% Wrap each of the function calls with a profile function
	NewAbs = wrap_form(Form),
	%io:format("~p~n", [NewAbs]),
	{ok, BinFilename, Bin} = compile:forms(NewAbs),
	file:write_file(BinFilename, Bin),

	% Run the script which prints the profile information out
	%erl_eval:exprs(NewAbs, []),
	ok.

%% @doc profile_wrapper will return the tokens for a wrapper function which
%% will print out the time each function call in escript took to run.
wrap_with_profiler({call, Line, _} = WrapAbs) ->
	% Im lazy so this is converting from the string and replacing needed data
	TimerFunString = "fun() -> 
					   	{Time, _} = timer:tc('$replace_fun'),
					   	io:format(\"Line=~p Time=~n\", ['$replace_line', Time)
			  		  end",
	{ok, Tokens, _} = erl_scan:string(TimerFunString),
	{ok, Abs} = erl_parse:parse(Tokens),
	% Find the atom data that needs to be replaced and replace it
	MapFun = fun({atom, _, '$replace_fun'}) -> 
					 WrapAbs;
		  		({atom, _, '$replace_line'}) -> 
					 {integer, Line, Line};
				(Acc) ->
					 Acc
			 end,
	erl_parse:map(MapFun, Abs).

%% @doc wrap_tokens will wrap all of the matching tokens with the wrapping
%% tokens.
wrap_form(Abs) ->
	WrapMapFun = fun({call, _, _} = FunAbs) -> 
						wrap_with_profiler(FunAbs);
					(AbsAcc) ->
						 io:format("~p~n", [AbsAcc]),
						AbsAcc
				 end,
	erl_parse:map_anno(WrapMapFun, Abs).
