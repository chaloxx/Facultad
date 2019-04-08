-module(ej5).
-compile(export_all).
-import(lists:(droplast,nth)).


suscriptor()->
    receive
        Msg -> io:fwrite("LLego un mensaje.. ~p",[Msg]),suscriptor();
        exit -> io:fwrite("Un subscriptor eliminado...~n")
    end.


difundir(Msg,Lista,N,M)->
    if
        N < M -> nth(N,Lista) ! Msg, difundir(Msg,Lista,N+1,M);
        true-> ok
    end.






servidor(Lista)->
    receive
        subscribir -> servidor(Lista++[spawn(?MODULE,suscriptor,[])]);
        {Msg}-> difundir(Msg,Lista,0,length(Lista));
        desubscribir -> last(Lista) ! exit,
                        servidor(droplast(Lista))
    end


init()->
    spawn(?MODULE,servidor,[[]]).