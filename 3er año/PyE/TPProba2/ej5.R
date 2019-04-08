states <- c("0","1","2","3","4","5","6")

v <- c(0,1/2,1/2,0,0,0,0,
       0,0,0,0,0,1,0,
       0,0,0,1,0,0,0,
       0,0,0,0,1,0,0,
       1,0,0,0,0,0,0,
       0,0,2/3,0,0,0,1/3,
       0,0,0,0,0,0,1)
m <- matrix(v,byrow=TRUE,nrow=7)

mc <- new("markovchain", transitionMatrix = m, states = states, name = "Raton")

is.irreducible(mc)
transientStates(mc)
absorbingStates(mc) 

jpeg("ej5.jpeg")
plot(mc)
dev.off()

#Como tiene un estado absorvente, quitamos la fila y la columna que corresponden
#a este estado para poder calcular el promedio de minutos que el raton se pasa
#recoriendo el laboratorio

id <- diag(1,6)
q <- m[-7,-7]
s <- solve(id-q) 

#Observemos que la primer fila de s es la suma del promedio de visitas
#a cada estado partiendo de 0, es decir la suma es el promedio de minutos que el raton
#pasa en el laberinto
#En este caso es 21

#Definimos una función para obtener el mismo resultado mediante simulación
promedio <- function(repet){
  suma = 0
  for(i in seq(repet)){
    v <- rmarkovchain(mc,n=300,t0="0")
    suma <- suma + length(v[v != "0" & v != "6"])
  }
  return(suma/repet)
}
  
promedio(100000)

#El promedio obtenido por simulacion se va estabilizando hacia el
#valor teorico obtenido por la matriz s
