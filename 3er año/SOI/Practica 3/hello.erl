-module(hello). 
-export([init/0, hello/0]).

hello() ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    helloloop().

helloloop() ->
    receive
        after 1000 -> ok
    end,
    io:format("Hello ~p~n",
        [case random:uniform(10) of 10 -> 1/uno; _ -> self() end]),
    helloloop().

init() -> spawn(?MODULE, hello, []).