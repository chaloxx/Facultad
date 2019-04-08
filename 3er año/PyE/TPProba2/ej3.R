library("ggplot2")
library(markovchain)
#a
CambioDePos <- function(p,n){
  transition <- matrix(c(1-p,p,1-p,p),byrow = TRUE, nrow = 2)
  states <- c("-1","1")
  
  mc <- new("markovchain",states = states,transitionMatrix = transition,name = "Emision de señales")
  return(rmarkovchain(mc,n =n,t0 = "1"))
}

#Dn representa el cambio de posición de una partícula, por lo tanto toma
#los valores 1 y -1.
y<-CambioDePos(0.3,5)
x<-1:length(y)
jpeg("ej3_a.jpeg")
qplot(x,y,xlab="Instantes de observación", ylab = "Cambio de posición de la partícula")
dev.off()
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

#Sn denota la posición de la partícula en el momento n.
y<-PosDeLaParticula(0.5,10)
x<-1:length(y)
jpeg("ej3_b.jpeg")
plot(x,y,type="b",xlab="Instantes de observación",ylab="Posición",main="Movimiento de una partícula")
dev.off()

