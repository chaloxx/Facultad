
-module(intro).
-compile(export_all).
%-record(persona,{nombre,apellido}).
match_test () -> 
  {A,B} = {5,4},%Bien
  %{C,C} = {5,4},%Este no es valido pues C no puede tomar 2 valores distintos
  {B,A} = {4,5},%Bien
  {D,D} = {5,5}.%Bien


tuple_test (P1,P2) ->
  io:format("El nombre de P1 es ~p y el apellido de P2 es ~p~n",[nombre(P1),apellido(P2)]).

apellido ({X,Y,{apellido,Z}}) -> Z.
    
nombre ({X,{nombre,Y},Z}) -> Y.



%The comparison operators work as follows: firstly, both sides of the
%operator are
%evaluated where possible (i.e. in the case when they are arithmetic
%expressions, or
%contain guard function BIFs); then the comparison operator is performed.
%For the purposes of comparison the following ordering is defined:
%number < atom < reference < port < pid < tuple < list


string_test () -> [
  helloworld == 'helloworld',%ambas son constantes o atoms
  "helloworld" < 'helloworld', 
  helloworld == "helloworld",
  [$h,$e,$l,$l,$o,$w,$o,$r,$l,$d] == "helloworld",% un string es una lista de char,cada char es un entero igual a su representación en ascii
  [104,101,108,108,111,119,111,114,108,100] < {104,101,108,108,111,119,111,114,108,100},
  [104,101,108,108,111,119,111,114,108,100] > 1,
  [104,101,108,108,111,119,111,114,108,100] == "helloworld"].


filtrar_por_apellido(Personas,Apellido) ->
  [X || {persona,{nombre,X},{apellido,Y}} <- Personas, Y == Apellido].


init () -> 
  P1 = {persona,{nombre,"Juan"},{apellido, "Gomez"}},
  P2 = {persona,{nombre,"Carlos"},{apellido, "Garcia"}},
  P3 = {persona,{nombre,"Javier"},{apellido, "Garcia"}},
  P4 = {persona,{nombre,"Rolando"},{apellido, "Garcia"}},
  %match_test(),
  tuple_test(P1,P2),
  string_test(),
  Garcias = filtrar_por_apellido([P4,P3,P2,P1],"Gomez").