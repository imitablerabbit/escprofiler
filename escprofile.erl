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

-type line() :: pos_integer().
-type mod_name() :: atom().
-type fun_name() :: atom().
-type args() :: list(erl_syntax:syntaxTree()).
-type call() :: {mod_name(), fun_name()} | fun_name().
-type app_info() :: {line(), call(), args()}.

main([Filename]) ->
	% Lex and parse the code via the preprocessor
	{ok, Form} = epp:parse_file(Filename, []),
	Tree = erl_syntax:form_list(Form),

	% Wrap each of the function calls with a profile function
	io:format("Editing Code:~n", []),
	NewForm = wrap_calls(Tree),
	NewAbs = erl_syntax:revert_forms(NewForm),

	% Print out the new wrapped code
	Io = erl_prettypr:format(NewForm),
	io:format("~nNew Code:~n", []),
	io:put_chars(Io),

	% Write the compiled bin to a file
	% This step can probably be skipped by just evaling the forms
	{ok, BinFilenameAtom, Bin} = compile:forms(NewAbs),
	BinFilename = atom_to_list(BinFilenameAtom) ++ ".tmp",
	file:write_file(BinFilename, Bin),

	% Execute the script file
	EscriptData = os:cmd("escript "++BinFilename),
	io:format("~nEscript IO Data:~n~s~n", [EscriptData]),

	% Read the data from the file
	{ok, TimeData} = file:read_file("escprofile.tmp"),
	io:format("~nProfile Data:~n~s~n", [TimeData]),

	% Delete the files made
	file:delete("escprofile.tmp"),
	file:delete(BinFilename),
	ok.

%% @doc wrap_form takes in a syntaxTree list and wraps any matching types
%% with a passed in syntaxTree.
-spec wrap_calls(Tree :: erl_syntax:syntaxTree()) -> erl_syntax:syntaxTree().
wrap_calls(Tree) ->
	Fun = fun(Node) ->
				  case erl_syntax:type(Node) of
					  application ->
						  AppInfo = application_information(Node),
						  print_app_info(AppInfo),
						  wrap_call(Node, AppInfo);
					  _ ->
						  Node
				  end
		  end,
	erl_syntax_lib:map(Fun, Tree).

%% @doc returns the data relating to an application node such as the
%% module, function, line number and the argument nodes used in the
%% application node.
-spec application_information(Node :: erl_syntax:syntaxTree()) -> app_info().
application_information(Node) ->
	Line = erl_syntax:get_pos(Node),
	AppOp = erl_syntax:application_operator(Node),
	Args = erl_syntax:application_arguments(Node),
	{Line, call_information(AppOp, erl_syntax:type(AppOp)), Args}.

%% @doc get the call information like the mod:fun or fun name 
-spec call_information(AppOp :: erl_syntax:syntaxTree(), AppOpType :: atom()) -> call().
call_information(AppOp, module_qualifier) ->
	ModNode = erl_syntax:module_qualifier_argument(AppOp),
	FunNode = erl_syntax:module_qualifier_body(AppOp),
	{erl_syntax:atom_value(ModNode), erl_syntax:atom_value(FunNode)};
call_information(AppOp, atom) ->
	erl_syntax:atom_value(AppOp).

%% @doc wrap a syntaxTree node with a timer block expresion and print out.
-spec wrap_call(Node :: erl_syntax:syntaxTree(), app_info()) ->	erl_syntax:syntaxTree().
wrap_call(Node, AppInfo) ->
	FormatString = format_app_info(AppInfo),
	Body = [
			% Add the timer function call
			erl_syntax:match_expr( 
				erl_syntax:tuple([erl_syntax:variable('Time'), erl_syntax:variable('Return')]),
				erl_syntax:application(
					erl_syntax:module_qualifier(erl_syntax:atom(timer), erl_syntax:atom(tc)),
					[erl_syntax:fun_expr([erl_syntax:clause(none, [Node])])])),

			% open the log file
			erl_syntax:match_expr( 
				erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:variable('File')]),
				erl_syntax:application(
					erl_syntax:module_qualifier(erl_syntax:atom(file), erl_syntax:atom(open)),
					[erl_syntax:string("escprofile.tmp"), erl_syntax:list([erl_syntax:atom(append)])])),

			% Format the time string
			erl_syntax:match_expr( 
				erl_syntax:variable('TimeString'),
				erl_syntax:application(
					erl_syntax:module_qualifier(erl_syntax:atom(io_lib), erl_syntax:atom(format)),
					[erl_syntax:string(FormatString ++ " took ~p~n"), erl_syntax:list([erl_syntax:variable('Time')])])),

			% Write the time data to the file
			erl_syntax:match_expr( 
				erl_syntax:atom(ok),
				erl_syntax:application(
					erl_syntax:module_qualifier(erl_syntax:atom(file), erl_syntax:atom(write)),
					[erl_syntax:variable('File'), erl_syntax:variable('TimeString')])),

			% Close the file
			erl_syntax:match_expr( 
				erl_syntax:atom(ok),
				erl_syntax:application(
					erl_syntax:module_qualifier(erl_syntax:atom(file), erl_syntax:atom(close)),
					[erl_syntax:variable('File')])),

		  	% Return the actual data
			erl_syntax:variable('Return')],
	erl_syntax:application(erl_syntax:fun_expr([erl_syntax:clause(none, Body)]), []).

%% @doc format the app info data into a string for printout
-spec format_app_info(app_info()) -> string().
format_app_info({Line, {Mod, Fun}, Args}) ->
	io_lib:format("~p:~p/~p on line ~p", [Mod, Fun, erlang:length(Args), Line]);
format_app_info({Line, Fun, Args}) ->
	io_lib:format("~p/~p on line ~p", [Fun, erlang:length(Args), Line]).

%% @doc print out the function call that is being wrapped
-spec print_app_info(AppInfo :: app_info()) -> ok.
print_app_info(AppInfo) ->
	FormatString = format_app_info(AppInfo),
	io:format("Wrapping call " ++ FormatString ++ "~n", []).
