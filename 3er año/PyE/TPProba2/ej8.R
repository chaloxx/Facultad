library("markovchain")

#a
#Matriz canonica
v <- c(1,0,0,0,
        0,0.2,0.8,0,
        0,0,0.3,0.7,
        1/2,0,0,1/2)
canonica <- matrix(v, byrow = TRUE, nrow = 4)
#b
#Llamaremos 4 al estado que representa que el alumno halla alcanzado su titulo
states <- c("4","1","2","3")
mc <- new("markovchain",transitionMatrix = canonica, states = states, name = "Estudiante")
plot(mc)
is.irreducible(mc)#No es irreducible
transientStates(mc)#Sus estados transitorios son 1,2 y 3
recurrentClasses(mc)#C1 = {4}
absorbingStates(mc)# 4



#c
sim <- function(mc,repet){
  valor <- 0
  for(i in 1:repet){
    v <- rmarkovchain(mc,n=2,t0 = "1")
    if (v[2] == "2"){
      valor <- valor+1
    }
  }
  return(valor/repet)
}


#Probabilidad de que un estudiante este en segundo año despues de dos examenes:
canonica2 <- canonica%*%canonica
#Suponiendo que antes de los 2 exámenes estaba en 1, 2 o 3:
sum(canonica2[,3])
#Suponiendo que partió del estado 1
prob <- sim(mc,1000)
canonica2[2,3]
#Observemos que el valor obtenido por simulación se aproxima al obtenido por
#la matriz teorica


#d
sim2 <- function(mc,repet){
  v <- rep(0,repet)
  for(i in 1:repet){
    s <- rmarkovchain(mc,n=15,t0="1")
    v[i] <- length(s[s != 4])+1
  }
  return(sum(v)/repet)
}

#
prom <- sim2(mc,10000)
q <- canonica[-1,-1]
id <- diag(1,3)
s <- solve(id-q)
sum(s[1,])
#Observemos que el valor obtenido por simulación tiende a acercarse 
#el valor obtenido por la matriz teorica
