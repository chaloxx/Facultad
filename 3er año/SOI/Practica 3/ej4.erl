-module (ej4).
-compile(export_all).


mensajero()->
    receive
        Pid -> mensajero(Pid)
    end.

mensajero(Pid)->
    receive
        {Msg,0} -> Pid ! exit, mensajero(Pid);
        {Msg,N} -> Pid ! {Msg,N-1},io:fwrite(" ~p ~n",[Msg]),mensajero(Pid);
        exit-> Pid ! exit
    end. 



init(N)->
    if N > 1 ->
        Id = spawn(?MODULE,mensajero,[]),
        init(N-1,{Id,Id,N})
    end.

init(N,{Primero,Ultimo,M})->
    if 
        N > 1 -> Pid = spawn(?MODULE,mensajero,[]),
                 Pid ! Ultimo,
                 init(N-1,{Primero,Pid,M});
        true ->  Pid = spawn(?MODULE,mensajero,[]),
                 Pid ! Ultimo,
                 Primero ! Pid,
                 Pid ! {"Probandoooo x0!",M}
    end.