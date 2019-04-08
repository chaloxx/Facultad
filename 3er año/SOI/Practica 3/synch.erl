-module(synch).
-export([testLock/0,testSem/0]).

%internal
-export([f/2,waiter/2]).
-export([waiter_sem/2,sem/2]).


lock (L) -> ok.
unlock (L) -> ok.
createLock () -> ok.
destroyLock (L) -> ok.

createSem (N) -> ok.
destroySem (S) -> ok.
semP (S) -> ok.
semV (S) -> ok.

f (L,W) -> lock(L),
  %   regioncritica(),
  io:format("uno ~p~n",[self()]),
  io:format("dos ~p~n",[self()]),
  io:format("tre ~p~n",[self()]),
  io:format("cua ~p~n",[self()]),
  unlock(L),
  W!finished.

waiter (L,0)  -> destroyLock(L);
waiter (L,N)  -> receive finished -> waiter(L,N-1) end.

waiter_sem (S,0)  -> destroySem(S);
waiter_sem (S,N)  -> receive finished -> waiter_sem(S,N-1) end.


testLock () -> L = createLock(),
  W=spawn(?MODULE,waiter,[L,3]),
  spawn(?MODULE,f,[L,W]),
  spawn(?MODULE,f,[L,W]),
  spawn(?MODULE,f,[L,W]),
  ok.
 
sem (S,W) -> 
  semP(S),
  %regioncritica(), bueno, casi....
  io:format("uno ~p~n",[self()]),
  io:format("dos ~p~n",[self()]),
  semV(S),
  W!finished.

testSem () -> S = createSem(2), % a lo sumo dos usando io al mismo tiempo
  W=spawn(?MODULE,waiter_sem,[S,5]),
  spawn(?MODULE,sem,[S,W]),
  spawn(?MODULE,sem,[S,W]),
  spawn(?MODULE,sem,[S,W]),
  spawn(?MODULE,sem,[S,W]),
  spawn(?MODULE,sem,[S,W]).