habitacion(oficina).
habitacion(baño).
habitacion(comedor).
habitacion(dormitorio).
habitacion(garage).
habitacion(cocina).


ubicacion(escritorio, oficina).
ubicacion(manzana, cocina).
ubicacion(luz_escritorio, escritorio).
ubicacion(lavadora, cocina).
ubicacion(control_remoto,'tv smart').
ubicacion('tv smart', comedor).
ubicacion(cereales, cocina).
ubicacion(computadora, oficina).


puerta(comedor, oficina).
puerta(comedor, garage).
puerta(comedor, cocina).
puerta(dormitorio, comedor).
puerta(bano, dormitorio).

comida(manzana).
comida(cereales).
here(oficina).

donde_comer(X, Y) :-  comida(X),ubicacion(X, Y).


conexion(X, Y) :- puerta(X, Y).
conexion(X, Y) :- puerta(Y, X).

list_objetos(Lugar) :- ubicacion(X, Lugar), tab(2), write(X), nl.

list_puertas(Lugar) :-  conexion(Lugar, X), tab(2), write(X), nl. 


info :- here(Lugar), write('Estas en la habitación: '), write(Lugar), nl, write('Cosas dentro de
la habitación: '),nl, list_objetos(Lugar), nl, write('Puedes ir a:'), nl, list_puertas(Lugar) . 