#!/usr/bin/env escript

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
	% Lex and parse the code via the preprocessor
	{ok, Form} = epp:parse_file(Filename, []),
	Tree = erl_syntax:form_list(Form),

	% Wrap each of the function calls with a profile function
	NewForm = wrap_calls(Tree),
	NewAbs = erl_syntax:revert_forms(NewForm),

	% Write the compiled bin to a file
	% This step can probably be skipped by just evaling the forms
	{ok, BinFilename, Bin} = compile:forms(NewAbs),
	file:write_file(BinFilename, Bin),

	% Execute the script file
	ok.

%% @doc wrap_form takes in a syntaxTree list and wraps any matching types
%% with a passed in syntaxTree.
wrap_calls(Tree) ->
	Fun = fun(Node) -> 
				  case erl_syntax:type(Node) of
					  application ->
						  {Line, Mod, Fun, Args} = application_information(Node),
						  io:format("Wrapping call ~p:~p/~p on line ~p~n", [Mod, Fun, erlang:length(Args), Line]),
						  wrap_node(Node);
					  _ ->
						  Node
				  end
		  end,
	erl_syntax_lib:map(Fun, Tree).

%% @doc returns the data relating to an application node such as the
%% module, function, line number and the argument nodes used in the
%% application node.
-spec application_information(Node :: erl_syntax:syntaxTree()) ->
	{Line :: pos_integer(), Mod :: atom(), Fun :: atom(), Args :: list(erl_syntax:syntaxTree())}.
application_information(Node) ->
	Line = erl_syntax:get_pos(Node),
	AppOp = erl_syntax:application_operator(Node),
	{Mod, Fun} = case erl_syntax:type(AppOp) of
					 module_qualifier ->
						 ModNode = erl_syntax:module_qualifier_argument(AppOp),
						 FunNode = erl_syntax:module_qualifier_body(AppOp),
						 {erl_syntax:atom_value(ModNode), erl_syntax:atom_value(FunNode)};
					 atom ->
						 {none, erl_syntax:atom_value(AppOp)}
				 end,
	Args = erl_syntax:application_arguments(Node),
	{Line, Mod, Fun, Args}.

%% @doc wrap a syntaxTree node with a timer block expresion and print out.
wrap_node(Node) ->
	erl_syntax:block_expr([
						   erl_syntax:application(
							 erl_syntax:module_qualifier(erl_syntax:atom(io), erl_syntax:atom(format)),[erl_syntax:string("About to send:~n")]),
						   Node
						  ]).
