-module(ejemplo2).
-compile(export_all).


aplica(F,X) -> F(F(X)).
%doble(N) -> 2*N.
init()->aplica(fun(N)->2*N end,90).

%init()-> aplica(fun(N)->3*N end,44).
%init()-> F = fun(X) when X > 10 -> 2*X; (X) -> 3*X end,
%aplica(F,45). 
