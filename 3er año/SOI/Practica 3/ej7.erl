-module(ej7).
-export([init/0,hello/0,daemon/0,helloloop/0]).

helloloop() ->
    receive
    after 1000 -> ok
    end,
    io:format("Jua jua jua jua ~p~n",[case random:uniform(10) of 10 -> 1/uno; _ -> self() end]),
    ?MODULE:helloloop().


    hello() ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    helloloop().


daemon()->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE,hello,[]),
    receive
        {Exit,From,Value} when From == Pid -> daemon()
    end.



init() ->
        spawn(?MODULE,daemon,[]). 

  