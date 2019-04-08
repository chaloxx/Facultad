-module(ejemplo6).
-compile(export_all).

%f()->io:format("f:~p~n",[self()]).
f2(X)->io:format("~p~n",[2*X]).

%init()->io:format("init:~p~n",[self()]),
%   [spawn(fun f/0) ||_ <-lists:seq(0,1000,1)],
%   chau.


init()->io:format("init:~p~n",[self()]),
   [f2(X)||X <-lists:seq(0,1000,1)],
   chau.