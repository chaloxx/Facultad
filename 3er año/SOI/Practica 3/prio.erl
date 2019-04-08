%~ Algo del preprocesador
%~ Tiene:

%~ -define(cosa,1).
%~ -undef(cosa).
%~ -ifdef(cosa).
%~ -ifndef(cosa),
%~ -endif


-module(prio).
-compile(export_all).
-define(MINP,1).
-define(MAXP,5).

prioread(Pid)->prioread(?MAXP,?MINP,Pid).

prioread(Min,Min,Pid)->
	receive{Min,Min}=M->Pid!M,prioread(Min,Min,Pid)
	after 0->ok
end;
prioread(Prio,Min,Pid)->
	receive{Prio,_}=M->Pid!M,prioread(Prio,Min,Pid)
	after 0->prioread(Prio-1,Min,Pid)
end.
	
	
mutex()->
	receive{lock,Pid}->Pid!ok end,
	receive{unlock,Pid1}when Pid1=:=Pid->Pid!ok end, 
	mutex().
	
lock(Mutex)->
	Mutex!{lock,self()},
	receive ok->ok end.
unlock(Mutex)->
	Mutex!{unlock,self()},
	receive ok->ok end.
