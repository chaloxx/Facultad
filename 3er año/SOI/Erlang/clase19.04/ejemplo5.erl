%-record(nombre,{elementos[=valor inicial]}).


-module(ejemplo5).
-compile(export_all).
-record(cosa,{nombre,edad,ciudad = "Rosario"}).
init()->
	R1 = #cosa{}, %valores por defecto
	R2 = #cosa{nombre="Juan",edad=22},
	R3=R2#cosa{edad=23},%copia y cambia de edad.
	io:format("~p~p~p~n",[R3#cosa.nombre,R3#cosa.edad,R3#cosa.ciudad]).
