-module(hola).
-compile(export_all).

hola() ->
	receive {Pid, X} -> io:format("~p:~p~n", [Pid, X]) end,
	hola().

init() ->
	register(cosa, spawn(hola, hola, [])).
