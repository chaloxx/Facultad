-module(ej6).
-compile(export_all).

%---------Mutex-----------------
bloq()->
    receive 
        {bloq,Pid} -> Pid ! {ok,self()}, bloq(Pid);
        destroy -> ok
    end,
    ok.

bloq(Pid)->
    receive
        {desbloq,Pid} -> Pid ! {ok,self()},bloq();
        destroy -> ok
    end,
    ok.


lock(L)->
    L ! {bloq,self()},
    receive {ok,L} ->  ok end,
    ok.

unlock(L)->
    L ! {desbloq,self()},
    receive {ok,L} -> ok end,
    ok.




createLock() -> spawn(?MODULE, bloq, []).

destroyLock(L) -> L ! destroy .




waiter (L,0) -> destroyLock(L);
waiter (L,N) ->
    receive
        finished -> waiter(L,N-1)
    end.
    


f (L,W) -> lock(L),
    %regioncritica(),
    io:format("uno ~p~n",[self()]),
    io:format("dos ~p~n",[self()]),
    io:format("tre ~p~n",[self()]),
    io:format("cua ~p~n",[self()]),
    unlock(L),
    W ! finished.


testLock () -> L = createLock(),
    W = spawn(?MODULE,waiter,[L,3]),
    spawn(?MODULE,f,[L,W]),
    spawn(?MODULE,f,[L,W]),
    spawn(?MODULE,f,[L,W]),
    ok.



%---------------Semaforo--------------------

semaforo(N) ->
    receive
       {pass,Pid} when N > 0 ->  Pid ! {ok,self()}, semaforo(N-1);
       {getOut,Pid} -> Pid ! {ok,self()}, semaforo(N+1);
       destroy -> ok
    end,
    ok.



createSem (N) -> spawn(?MODULE,semaforo,[N]).


destroySem (S) -> S ! destroy, ok.

semP (S) -> 
            S ! {pass,self()},
            receive {ok,S} -> ok end,
            ok.

semV (S) -> 
            S ! {getOut,self()},
            receive {ok,S} -> ok end,
            ok.




waiter_sem (S,0) -> destroySem(S);
waiter_sem (S,N) -> 
    receive
         finished -> waiter_sem(S,N-1)
    end,
    ok.

    



sem (S,W) ->
    io:format("aca llego ~p~n",[self()]),
    semP(S),
    %regioncritica(), bueno, casi....
    io:format("uno ~p~n",[self()]),
    io:format("dos ~p~n",[self()]),
    semV(S),
    W!finished.
    
testSem () -> S = createSem(2), % a lo sumo dos usando io al mismo tiempo
    W = spawn(?MODULE,waiter_sem,[S,6]),
    spawn(?MODULE,sem,[S,W]),
    spawn(?MODULE,sem,[S,W]),
    spawn(?MODULE,sem,[S,W]),
    spawn(?MODULE,sem,[S,W]),
    spawn(?MODULE,sem,[S,W]),
    spawn(?MODULE,sem,[S,W]).