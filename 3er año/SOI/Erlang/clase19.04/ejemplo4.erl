-module(ejemplo4).
-compile(export_all).


permuts([])->[[]];
permuts(L) -> [[X|Y] || X<-L, Y<-permuts(L--[X])].
