from itertools import chain, combinations

def powerset(s):
    l = list(s)
    c = chain.from_iterable(combinations(l,r) for r in range(1, len(l)+1))
    return list(map(set, list(c)))

def armstrong(F,R):
    partesR = powerset(R.copy())
    resultado = F[:]
    
    # Aplica reglas de transitividad
    for x in partesR:
        for y in powerset(x):
            resultado += [(x, y)]

    cambios = True
    while(cambios):
        cambios = False
        agregar = []

        for (x,y) in resultado:
            # Aplica las reglas de aumentatividad
            for z in partesR:
                 u1 = z | x
                 u2 = z | y
                 if (u1,u2) not in resultado and (u1,u2) not in agregar:
                    agregar += [(u1,u2)]
        
            # Aplica las reglas de transitividad
            for (w,z) in resultado:
                if y == w and (x,z) not in resultado and (x,z) not in agregar:
                    agregar += [(x,z)]
                    cambios = True
        
        resultado += agregar
    return resultado

def cierre(F,R):
    dependencias = armstrong(F[:],R.copy())
    resultado = R.copy()
    
    cambios = True
    while (cambios):
        cambios = False
        for (b,c) in dependencias:
            u = resultado | c
            if b.issubset(resultado) and resultado != u:
                resultado = u
                cambios = True
    return resultado

def superset(x,y):
    for s in y:
       if x.issuperset(s):
          return True
    return False

def keys(F,R):
    partes = powerset(R.copy())
    partes.sort(key=len)
    resultado = []
    for x in partes:
         if cierre(F[:],x.copy()) == R and not(superset(x.copy(),resultado[:])):
            resultado += [x]
    return resultado

R1 = {"A", "B", "C", "D"}
F1 = [({"A"},{"B"}), ({"B"}, {"A","D"}), ({"B","C"}, {"A"})]

R2 = {"A", "B", "C", "D", "E", "F"}
F2 = [({"A","B"}, {"C"}), ({"B","D"}, {"E","F"})]

R3 = {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J"}
F3 = [({"A"},{"I"}), ({"A","B"},{"C"}), ({"A","D"},{"G","H"}), ({"B","D"},{"E","F"}), ({"H"},{"J"})]
A3 = {"B", "D"}

R4 = {"A", "B", "C", "D", "E", "F", "G", "H"}
F4 = [({"A"},{"B","C"}), ({"C"},{"D"}), ({"D"},{"G"}), ({"E"},{"A"}), ({"E"},{"H"}), ({"H"},{"E"})]
A4 = {"A", "B", "C", "D", "G"}

R5 = {"A", "B", "C", "D", "E", "F", "G"}
F5 = [({"A"},{"F"}), ({"A"},{"G"}), ({"B"}, {"E"}), ({"C"},{"D"}), ({"D"},{"B"}), ({"E"}, {"A"}), ({"F","G"},{"C"})]
A5 = {"F", "G"}



def test_powerset2():
    assert powerset([]) == []
    assert powerset(["A"]) == [{"A"}]
    assert powerset(["A","B"]) == [{"A"}, {"B"}, {"A","B"}]
    assert powerset(["A","B","C"]) == [{"A"}, {"B"}, {"C"}, {"A","B"}, {"A", "C"}, {"B","C"}, {"A","B","C"}]

def test_armstrong():
    assert len(armstrong(F1, R1)) == 137
    assert len(armstrong(F2, R2)) == 1081

def test_cierre():
    assert cierre(F3, A3) == {"B", "D", "E", "F"}
    assert cierre(F4, A4) == {"A", "B", "C", "D", "G"}
    assert cierre(F5, A5) == {"A", "B", "C", "D", "E", "F", "G"}


def test_keys():
    assert keys(F3, R3) == [{"A","B","D"}]
    assert keys(F4, R4) == [{"E","F"}, {"F","H"}]
    assert keys(F5, R5) == [{"A"}, {"B"}, {"C"}, {"D"}, {"E"}, {"F","G"}]

