% Ej 7
% t(x,y) : x tiene como mascota a y
% l(x) : x ama a los animales
% m(x,y) : x mató a y

t(juan,perro).
t(pedro,perro).

m(juan,iris).
m(pedro,iris).
m(maria,iris).

l(X,Y) :- t(X,Y).

not(m(X,Y)) :- l(X,Y).


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
ubicacion(control_remoto, 'tv_smart').
ubicacion('tv smart', comedor).
ubicacion(cereales, cocina).
ubicacion(computadora, oficina).


puerta(comedor, oficina).
puerta(comedor, garage).
puerta(comedor, cocina).
puerta(dormitorio, comedor).
puerta(baño, dormitorio).

comida(manzana).
comida(cereales).
here(oficina).
