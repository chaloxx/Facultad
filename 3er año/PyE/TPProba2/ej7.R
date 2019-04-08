library(ggplot2)
library(plyr)


ProcesoPois<- function(t,lambda){
  #Obtenemos un valor aleatorio mediante distribución de Poisson
  #Este valor determina la cantidad de eventos que ocurren hasta t
  N<- rpois(1,t*lambda) 
  #Generamos N valores de forma aleatoria con distribución uniforme
  #Cada valor generado representa en que tiempo sucedió un evento
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
jpeg("ej7_a.jpeg")
PlotPois(sim)
dev.off()


#b
DensidadT <- function(t,n){
  lambda <- as.character(v[n])
  legenda <- c(paste("Distribución exponencial con parámetro",lambda,sep = " "),"Curva de densidad de los tiempos")
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
#Comparamos con la función de densidad teorica
#que sigue una distribución exponencial con mismo lambda
jpeg("ej7_b1.jpeg")
DensidadT(t1,1)
dev.off()
jpeg("ej7_b2.jpeg")
DensidadT(t2,2)
dev.off()


1-exp(-1/2*2)
length(t1[t1 <=2]) / length(t1)
