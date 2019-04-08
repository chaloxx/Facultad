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
    try <-rmarkovchain(mc,n=10000,t0= as.character(estadoInicial))
    if(length(try[try=="0"])!=0) valores <- valores + 1
  }
  return(valores/repet )
}


#Supongamos que el jugador A tiene una probabilidad de 0.8 de ganar en 
#cada partida y que entre A y B el capital es de 5


#a-
#Realizamos una trayectoria y vemos como evoluciona el capital del jugador a
mc <- new("markovchain", transitionMatrix = markovMatrix(5,0.8), states = construirEstados(5)) 
jugadas<- rmarkovchain(mc,n=100,t0= as.character("3"))
pos <- length(jugadas[jugadas != 5 & jugadas != 0]) + 1
library(ggplot2)
x <- 1:pos; y = jugadas[1:pos]
jpeg("ej1_a.jpeg")
qplot(x,y,xlab = "Nro. de Jugada", ylab= "Evolución de Capital",geom = c("point") ) 
dev.off()


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
#Lo corroboramos mediante simulación:
ProbDeRuina(3,mc,100,1000)
#Observemos que el numero obtenido en la simulación se apróxima al obtenido
#por la matriz teórica.
