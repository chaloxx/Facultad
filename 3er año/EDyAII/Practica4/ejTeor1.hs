
type Cola a = C [a]
vacia = []


poner::a -> Cola a -> Cola a
poner d c = d:c

primero::Cola a -> a
primero vacia = error "La cola esta vacia"
primero xs = last xs


sacar::Cola a -> Cola a
sacar vacia = error "La cola esta vacia"
sacar xs = init xs









    