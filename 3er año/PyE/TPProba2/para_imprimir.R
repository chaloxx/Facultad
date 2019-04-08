#Ej1
library(markovchain)

markovMatrix <- function(capital,prob){
  cantEstados <- capital+1
  pi <- matrix(rep(0,cantEstados^2), byrow = TRUE, nrow = cantEstados)
  pi[1,1] <- 1
  for(i in seq(2,cantEstados-1)){
    for(j in seq(1,cantEstados)){
      if (j == i-1) pi[i,j] <- 1-prob
      else if (j == i+1) pi[i,j] <- prob
    }
  }
  pi[cantEstados,cantEstados] <- 1
  return(pi)
}

construirEstados <- function(capital){
  estados <- seq(0,capital)
  for(i in seq(1,capital+1)){
    estados[i] <- as.character(estados[i])
  }
  return(estados)
}


ProbDeRuina <- function(estadoInicial,mc,jugadas,repet){
  valores <- 0
  for(i in seq(1,repet)){
    try <-rmarkovchain(mc,n = 10000,t0 = as.character(estadoInicial))
    if(length(try[try=="0"])!= 0) valores <- valores + 1
  }
  return(valores/repet )
}
estados <- seq(0,capital)
for(i in seq(1,capital+1)){
  estados[i] <- as.character(estados[i])
}


#a-
#Supongamos que el jugador A tiene una probabilidad de 0.8 de ganar en 
#cada partida y que entre A y B el capital es de 5
#Realizamos una trayectoria y vemos como evoluciona el capital del jugador a
mc <- new("markovchain", transitionMatrix = markovMatrix(5,0.8), states = construirEstados(5)) 
jugadas<- rmarkovchain(mc,n=100,t0= as.character("3"))
pos <- length(jugadas[jugadas != 5 & jugadas != 0]) + 1
library(ggplot2)
x <- 1:pos; y = jugadas[1:pos]
qplot(x,y,xlab = "Nro. de Jugada", ylab= "Evolucion de Capital",geom = c("point") ) 

#Armamos la matriz b
v1 <- c(0.2,0,
        0,0,
        0,0,
        0,0.8)

b <- matrix(v1, byrow = TRUE, nrow = 4)

#Armamos la matriz q y luego la s
v2 <- c(0,0.8,0,0,
        0.2,0,0.8,0,
        0,0.2,0,0.8,
        0,0,0.2,0)

q <- matrix(v2,byrow = TRUE, nrow = 4)
id <- diag(1,4)
s <- solve(id-q)

#Por ultimo obtenemos la matriz g
g <- s%*%b

#Probabilidad de ruina del jugador A:
g[3,1]
#Lo corroboramos mediante simulacion:
ProbDeRuina(3,mc,100,1000)
#Observemos que el numero obtenido en la simulacion se aproxima al obtenido
#por la matriz teorica.



#Ej2
library(markovchain)
library(ggplot2)


#a
#En: la señal emitidad en el momento n es correcta o incorrecta
bernoulli <- function(p,n){
  m <- matrix(c(1-p,p,1-p,p),byrow = TRUE, nrow = 2)
  states <- c("0","1")
  mc <- new("markovchain",states = states,transitionMatrix = m,name = "Emision de señales")
  return(rmarkovchain(mc,n =n,t0 = "0"))
}


#Realizacion del proceso de bernoulli con n = 10 y p = 0.5
y<-bernoulli(0.5,10)
x<-1:length(y)
qplot(x,y,xlab = "Instante de emision",ylab = "Correcto",geom="point")



#b
#Sn: Numero de señales incorrectas emitidas en el momento n
toChar <- function(v,long){
  for(i in seq(1,long)){
    v[i] <- as.character(v[i])
  }
  return(v)
}

transition <- function(p,long){
  t <-matrix(rep(0,long^2),byrow = TRUE, nrow = long)
  for(i in seq(1,long-1)){
    for(j in seq(1,long)){
      if(i == j) t[i,j] <- 1-p
      else if (j == i+1) t[i,j] <- p
    }
  }
  t[long,long] <- 1
  return(t)
}



bernoulli2 <- function(p,n){
  mc <- new("markovchain", states = toChar(seq(0,n),n+1), transitionMatrix =transition(p,n+1)  ,name="Cantidad de Señales Emitidas")
  return(rmarkovchain(mc,n=n,t0 = "0"))
}

y<-bernoulli2(0.5,10)
x<-0:(length(y)-1)
plot(x,y,type="b",xlab="Instante de observacion",ylab="Nro. de señales emitidas incorrectamente")

#c
#Un proceso de bernoulli es una sucecion de variables aleatorias independientes
#que siguen una distribucion de bernoulli, por lo tanto su esperanza es p.
#Por lo mismo, su varianza es p*(1-p)



#Ej3
library("ggplot2")
library(markovchain)
#a
CambioDePos <- function(p,n){
  transition <- matrix(c(1-p,p,1-p,p),byrow = TRUE, nrow = 2)
  states <- c("-1","1")
  mc <- new("markovchain",states = states,transitionMatrix = transition,name = "Emision de señales")
  return(rmarkovchain(mc,n =n,t0 = "1"))
}

#Dn representa el cambio de posicion de una particula, por lo tanto toma
#los valores 1 y -1.
y<-CambioDePos(0.3,10)
x<-1:length(y)
qplot(x,y,xlab = "Instantes de observacion",ylab = "Cambio de posicion de la particula")
#E(Dn) = E(2*In-1) = 2*E(In) - E(1) = 2*p-1
#V(Dn) = V(2In-1) = 4*V(In) + V(1) = 4*(p*(1-p)) 


#b


toChar <- function(v,long){
  for(i in seq(1,long)){
    v[i] <- as.character(v[i])
  }
  return(v)
}

transition <- function(p,long){
  t <-matrix(rep(0,long^2),byrow = TRUE, nrow = long)
  t[1][1] <- 1-p
  t[1,2] <- p
  for(i in seq(2,long-1)){
    for(j in seq(1,long)){
      if(j == i-1) t[i,j] <- 1-p
      else if (j == i+1) t[i,j] <- p
    }
  }
  t[long,long-1] <- 1-p
  t[long,long] <- p
  return(t)
}



PosDeLaParticula <- function(p,n){
  mc <- new("markovchain", states = toChar(seq(0,n),n), transitionMatrix =transition(p,n+1)  ,name="Cantidad de Señales Emitidas")
  return(rmarkovchain(mc,n=n,t0 = "0"))
}

#Sn denota la posicion de la particula en el momento n.
y<-PosDeLaParticula(0.5,10)
x<-1:length(y)
plot(x,y,type="b",xlab="Instantes de observacion",ylab="Posicion",main="Movimiento de una particula")


#Ej4
#a
library("markovchain")
v <- c(0,1/2,1/2,0,0,
       1/5,1/5,1/5,1/5,1/5,
       1/3,1/3,0,1/3,0,
       0,0,0,0,1,
       0,0,1/2,1/2,0)





m <- matrix(v,byrow = TRUE, nrow= 5)
states <- c("1","2","3","4","5")
mc <- new("markovchain",states = states,transition = m,name="Page Rank")
plot(mc)

#Calcular distribucion estacionaria
id <- diag(1,5)
ecu <- rbind((t(m)-id)[-5,],c(1,1,1,1,1))
pi <-solve(ecu,c(0,0,0,0,1))
#Comparamos con el resultado que nos provee R
steadyStates(mc)
#Ademas pi = pi*m
pi-(pi%*%m) < 1*10^(-10)
#Las probabilidades de imprimir cada pagina estan dadas por pi



#Ej5

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

plot(mc)

#Como tiene un estado absorvente, quitamos la fila y la col que corresponden
#a este estado para poder calcular el promedio de minutos que el raton se pasa
#recoriendo el laboratorio

id <- diag(1,6)
q <- m[-7,-7]
s <- solve(id-q) 

#Observemos que la primer fila de s es la suma del promedio de visitas
#a cada estado partiendo de 0, es decir la suma es el promedio de minutos que el raton
#pasa en el laberinto
#En este caso es 21

#Definimos una funcion para obtener el mismo resultado mediante simulacion
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



#Ej6
library(markovchain)

#a
#I~Poi(lambda)
#         |max(1,bmt(n)) con P(I = 0)
#bmt(n+1)=| 
#         |min(5,bmt(n)+2*I) con P(I >= 1)


#b
#Para representar el grafo utilizaremos el valor de lambda dado en c:
lambda <- 5/100
p <-exp(-lambda) #P(I = 0)
q <- exp(-lambda)*lambda # P(I = 1)


v <- c(p,0,q,0,1-(p+q),
       p,0,0,q,1-(p+q),
       0,p,0,0,1-p,
       0,0,p,0,1-p,
       0,0,0,p,1-p)

m <- matrix(v,byrow = TRUE,nrow = 5)
mc <- new("markovchain",transitionMatrix = m, states = c("1","2","3","4","5"),name="Bonus Malus")
plot(mc)



#c
#Observemos que la cadena es aperiodica, irreducible y aperiodica
is.irreducible(mc)
period(mc) #Si el periodo es 1 entonces la cadena es aperiodica
transientStates(mc) # Si el cjto de estados transitorios esta vacio, la cadena es recurrente
#Por lo tanto existe una unica distribucion estacionaria para esta cadena.
#Calcular distribucion estacionaria
id<- diag(1,5)
unos <- c(1,1,1,1,1)
b <- c(0,0,0,0,1)
m2 <- rbind((t(m)-id)[-5,],unos)
pi <- solve(m2,b)
#Comparar con el resultado en r
steadyStates(mc)
#Calcular promedio prima anual
primas <- c(0.50,0.70,0.90,1,1.25)
#Definamos la variable aleatoria x: prima anual que paga un asegurado.
#Calculamos la esperanza de esta variable
esp<- sum(primas*pi) 


#Ej7
library(ggplot2)
library(plyr)


ProcesoPois<- function(t,lambda){
  #Obtenemos un valor aleatorio mediante distribucion de Poisson
  #Este valor determina la cantidad de eventos que ocurren hasta t
  N<- rpois(1,t*lambda) 
  #Generamos N valores de forma aleatoria con distribucion uniforme
  #Cada valor generado representa en que tiempo sucedio un evento
  C<- sort(runif(N,0,t)) 
  data.frame(tiempo=c(0,0,C),nroEvento=c(0,0:N)) 
}

NPois<-function(t,means){
  n <- length(means)
  #Crear dataframes de las simulaciones
  C <- lapply(1:n, function(n) data.frame(ProcesoPois(t,means[n]),simulacion=n)) #Genera N dataframes con los procesos
  #Unir en una sola dataframe
  C <- ldply(C, data.frame) 
  return(C)
}

PlotPois<- function(C){
  C$simulacion<-factor(C$simulacion) #Convierte en factores
  qplot(tiempo,nroEvento,data=C,geom=c("step", "point"),color=simulacion,xlab="Tiempo",ylab="Nro. de Eventos",main="Proceso de Poisson")
}

#a
#Tomamos un vector de lambdas
v <- c(1/2,3/4)
#Simulamos las trayectorias en un intervalo de tiempo [0,570]
sim <- NPois(21,v)
#Representamos graficamente su comportamiento
PlotPois(sim)


#b
DensidadT <- function(t,n){
  lambda <- as.character(v[n])
  legenda <- c(paste("Distribucion exponencial con parametro",lambda,sep = " "),"Curva de densidad de los tiempos")
  x<-0:max(t)
  hist(t,freq = FALSE,xlab = "Tiempos",ylab = "Densidad",main="Densidad de los tiempos para lambda = "%+%lambda)
  lines(density(t1),col = "blue")
  lines(v[n]*exp(-v[n]*x), col = "red")
  legend("topright",col=c("red","blue"),legend =legenda,lwd=1, bty = "n")
}


#Para cada lambda simulamos una trayectoria con un tiempo mayor
sim <- NPois(2000,v)
#Calculamos los tiempos
tiempos <- ProcesoPois(7,1/2)

t1 <- diff(sim[sim$simulacion == 1,]$tiempo)
t2 <- diff(sim[sim$simulacion == 2,]$tiempo)
#Representamos la densidad de los tiempos entre llegadas a travez de histogramas
#Comparamos con la funcion de densidad teorica
#que sigue una distribucion exponencial con mismo lambda
DensidadT(t1,1)
DensidadT(t2,2)


#Ej8
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
#Definimos una funcion para calcular la probabilidad de estar en segundo anio
#despues de dos examenes
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
#Suponiendo que antes de los 2 examenes estaba en 1, 2 o 3:
sum(canonica2[,3])
#Suponiendo que partio del estado 1
prob <- sim(mc,1000)
canonica2[2,3]
#Observemos que el valor obtenido por simulacion se aproxima al obtenido por
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
#Observemos que el valor obtenido por simulacion tiende a acercarse 
#el valor obtenido por la matriz teorica





