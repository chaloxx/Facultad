-module(ejemplo).
-compile(export_all).

    romano(0)-> "Error";
    romano(N) when N > 0 ->
	   Digs = [{"M",1000}, {"CM",900},{"D",500},{"CD",400},{"C",100},
			 {"XC",90},{"L",50},{"XL",40},{"X",10},{"IX",9},
			 {"V",5},{"IV",4},{"I",1}],
			 romano(N,Digs).
	romano(0,_) -> "";
	romano(N, [{R,D}|_]=L) when N >= D -> io:fwrite("2c~p ",[N]), R++romano(N-D,L);
	romano(N,[_|T]) -> io:fwrite("3c~p ",[N]),romano(N,T).
