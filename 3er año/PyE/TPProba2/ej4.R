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
jpeg("ej4.jpeg")
plot(mc)
dev.off()

id <- diag(1,5)
ecu <- rbind((t(m)-id)[-5,],c(1,1,1,1,1))
pi <-solve(ecu,c(0,0,0,0,1))
#Comparamos con el resultado que nos provee R
steadyStates(mc)
#Además pi = pi*m
pi-(pi%*%m) < 1*10^(-10)
#Las probabilidades de imprimir cada página estan dadas por pi
