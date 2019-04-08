-module(eco).
-compile(export_all).
                          
server() ->
    {ok, ListenSocket} = gen_tcp:listen(8000, [{active, false}]),
    wait_connect(ListenSocket, 1).

wait_connect(ListenSocket, N) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, wait_connect, [ListenSocket, N+1]),
    get_request(Socket, N).

get_request(Socket, N) -> ok. % Atender al cliente.