 -module(ejemplo8).
 -compile(export_all).

%~ loop(F)->
	%~ receive  {swap,FF} ->loop(FF);
	%~ {Pid,X} -> Pid ! F(X), loop(F);
	%~ fin->ok
	%~ end.
	
	
%~ init() -> Pid = spawn(?MODULE, loop, [fun(N)->2*N end]),

	%~ Pid ! {self(),100},
	%~ io:format("~p~n",[receive X->X end]),
	%~ Pid ! {swap,fun(N)->N*N end},
	%~ Pid ! {self(),100},
	%~ io:format("~p~n",[receive X1->X1 end]),
	%~ Pid ! fin.
	
-define(MINP,1).
-define(MAXP,5).

prioread(Pid)->prioread(?MAXP,?MINP,Pid).
prioread(Min,Min,Pid)->
	receive {Min,_} = M -> Pid ! M, prioread(Min,Min,Pid)
	after 0 -> ok
	end;
prioread(Prio,Min,Pid)->
	receive {Prio,_} = M -> Pid ! M, prioread(Prio,Min,Prio)
	after 0 -> prioread(Prio-1, Min, Pid)
	end.
	
mutex() ->
	receive {lock,Pid} -> Pid ! ok end,
	receive {unlock,Pid1} when Pid1 =:= Pid ->Pid1 ! ok end,
	mutex().
	
lock(Mutex)->
	Mutex ! {lock,self()},
	receive ok -> ok end.
unlock(Mutex)->
	Mutex ! {unlock,self()},
	receive ok -> ok end.
