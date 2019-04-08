

-module(ej3).
-compile(export_all).
                          
server() ->
    {ok, ListenSocket} = gen_tcp:listen(8000, [list,{active, false}]),%para recivir mensajes como paquetes en el inbox {active,true}
    wait_connect(ListenSocket, 1).

wait_connect(ListenSocket, N) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, wait_connect, [ListenSocket, N+1]),
    get_request(Socket,N),
    ok.

get_request(Socket, N) ->
   %receive
    %  {tcp,Socket,Msg} -> gen_tcp:send(Socket,Msg)
   %end,
   {ok,Msg} = gen_tcp:recv(Socket,0),
   gen_tcp:send(Socket,Msg),
   ok.
