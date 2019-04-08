-module(ejemplo3).
-compile(export_all).

%loop(N,N) -> io:format("~p~n",[N]);
%loop(M,N)->
	%io:format("~p~n",[M]),
	%loop(M+1,N).
%init(N) -> loop(0,N).



%[(x,y) | x<-[1,2,3], y <-[10,11,13], x >y]

fun mapend _[] = []
    meapend f(n::t) = f(h)@map f+
map:('a->'b)->'a list
