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


#Realización del proceso de bernoulli con n = 10 y p = 0.5
y<-bernoulli(0.5,5)
x<-1:length(y)
jpeg("ej2_a.jpeg")
qplot(x,y,xlab = "Momento de emisión", ylab = "",geom="point")
dev.off()


#b
#Sn: Número de señales incorrectas emitidas en el momento n
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

y<-bernoulli2(0.5,5)
x<-1:length(y)
jpeg("ej2_b.jpeg")
plot(x,y,type="b",xlab="Instante de observación",ylab="Nro. de señales emitidas incorrectamente")
dev.off()
#c
#Un proceso de bernoulli es una suceción de variables aleatorias independientes
#que siguen una distribución de bernoulli, por lo tanto su esperanza es p.
#Por lo mismo, su varianza es p*(1-p)



