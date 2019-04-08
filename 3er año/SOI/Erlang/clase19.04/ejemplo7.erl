-module(ejemplo7).



%f(X,Y,Z)->%debe exportarse
%	io:format("~p:~p~p~p~n",[self(),X,Y,Z]).
%	init()->spawn(ejemplo7,f,[123,"hola",true]).% o mejor aun spwan(?MODULE,f,[123,"hola",true])


-compile(export_all).

f()->
	receive {Pid,p1,N}-> io:format("~p~n",[2*N]), Pid ! "Bueno", f();
	        {Pid,p2,N}-> io:format("~p~n",[3*N]), Pid ! "Ufa", f();    
	        fin->ok
	        
	end.

init()->
	Pid = spawn(?MODULE,f,[]),
	Pid !{self(),p2,10},
	receive S->io:format("~p~n",[S])end,
	Pid ! {self(),p1,35},
	receive S1->io:format("~p~n",[S1])end,
	Pid ! fin.
