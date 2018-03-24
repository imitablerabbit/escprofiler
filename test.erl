-module(test).
-author(markhillman).
-export([main/1]).

main([]) ->
	io:format("hello world", []),
	timer:sleep(10),
	Variable = hello(),
	Variable = hello,
	ok.

hello() -> 
	hello.
