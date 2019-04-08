-module(ej2).
-compile(export_all).


wait(Time) ->
   receive
   after
    Time -> ok
   end.



cronometro(Fun,Hasta,Periodo)->
     if 
        Hasta > Periodo -> Fun(),wait(Periodo),cronometro(Fun,Hasta-Periodo,Periodo);
        true -> ok
    end.


second_to_milisecond(Second)->
    Second*1000.

init()->
    %wait(second_to_milisecond(10)).
    cronometro(fun () -> io:format("Tick~n") end,60000,5000).
