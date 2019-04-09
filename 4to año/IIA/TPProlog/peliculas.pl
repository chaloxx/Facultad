/*Sistema experto
Dominio: Películas
Comenzar con init.*/



init :- hipotetizar(Pelicula),(Pelicula == 'desconocido' -> write('No se encontraron recomendaciones');
                               write('Te sugiero: '),write(Pelicula))
                               ,nl,deshacer.

:- dynamic si/1,no/1.



/*Catálogo de peliculas de terror*/
hipotetizar('Chuky, el munieco maldito') :- terror,brujeria,muniecos,verificar(cuchilla,objeto),verificar(cicatrices,caracteristica).
hipotetizar('Dolly, la munieca embrujada') :- terror,brujeria,muniecos,verificar(cuchilla,objeto).
hipotetizar('La Casa Oscura') :- terror,brujeria,casas,verificar(retro,epoca).
hipotetizar('Amityville: El despertar') :- terror,brujeria,casas,verificar(actual,epoca).
hipotetizar('El exorcista') :- terror,posesiones,personas,verificar(cicatrices,caracteristica),verificar(retro,epoca).
hipotetizar('El rito') :- terror,posesiones,personas,verificar(actual,epoca).
hipotetizar('La masacre de texas') :- terror, asesinos,verificar(retro,epoca),verificar(mascara,objeto).
hipotetizar('Zodiaco') :- terror, asesinos.
hipotetizar('desconocido').



/* hacer preguntas */
preguntar(Consulta,genero) :-
 write('Te gustan las peliculas de '), write(Consulta), write('? '), read(Respuesta), nl,
 ( (Respuesta == si) -> assert(si(Consulta)) ; assert(no(Consulta)), fail).


 preguntar(Consulta,epoca) :-
  write('Te gustan las peliculas de la epoca '), write(Consulta), write('? '), read(Respuesta), nl,
  ( (Respuesta == si) -> assert(si(Consulta)) ;assert(no(Consulta)), fail).


 preguntar(Consulta,objeto) :-
   write('Prefieres que el protagonista use '), write(Consulta), write('? '), read(Respuesta), nl,
   ( (Respuesta == si) -> assert(si(Consulta)) ;assert(no(Consulta)), fail).


preguntar(Consulta,caracteristica) :-
  write('Prefieres que el protagonista tenga '), write(Consulta), write('? '), read(Respuesta), nl,
  ( (Respuesta == si) -> assert(si(Consulta)) ;assert(no(Consulta)), fail).




/*Reglas*/

/*Primer nivel*/
terror :- verificar(terror,genero).

/*Segundo nivel*/
brujeria :- verificar(brujeria,genero).
posesiones :- verificar(posesiones,genero).
asesinos :- verificar(asesinos,genero).

/*Tercer nivel*/
muniecos :- verificar('muniecos embrujados',genero).
personas :- verificar('personas poseidas',genero).
casas :- verificar('casas embrujadas',genero).


 /* Verificar algo*/
 verificar(Consulta,Clase) :- (si(Consulta) -> true ; (no(Consulta) -> fail ; preguntar(Consulta,Clase))).



/* deshacer  si/no aserciones */
deshacer :- retract(si(_)),fail.
deshacer :- retract(no(_)),fail.
deshacer.
