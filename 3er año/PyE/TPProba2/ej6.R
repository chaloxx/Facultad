library(markovchain)

#a
#I~Poi(lambda)
#         |max(1,bmt(n)) con P(I = 0)
#bmt(n+1)=| 
#         |min(5,bmt(n)+2*I) con P(I >= 1)


#b
#Para representar el grafo utilizaremos el valor de lambda dado en c:
lambda <- 5/100
p <- exp(-lambda) #P(I = 0)
q <- exp(-lambda)*lambda # P(I = 1)
r <- 1-(p+q)
s <- 1-p

v <- c(0.9512294,0,0.04756147,0,0.001209104,
       0.9512294,0,0,0.04756147,0.001209104,
       0,0.9512294,0,0,0.04877058,
       0,0,0.9512294,0,0.04877058,
       0,0,0,0.9512294,0.04877058)

v <- c(1/4,0,1/4,0,1/2,
       1/4,0,0,1/2,1/4,
       0,1/2,0,0,1/2,
       0,0,1/2,0,1/2,
       0,0,0,1/2,1/2)
  

m <- matrix(v,byrow = TRUE,nrow = 5)
mc <- new("markovchain",transitionMatrix = m, states = c("1","2","3","4","5"))
jpeg("ej6.jpeg")
plot(mc)
dev.off()


#Observemos que la cadena es aperiodica, irreducible y aperiodica
is.irreducible(mc)
period(mc) #Si el periodo es 1 entonces la cadena es aperiódica
transientStates(mc) # Si el cjto de estados transitorios está vacio, la cadena es recurrente
#Por lo tanto existe una única distribución estacionaria para esta cadena.
#Calcular distribución estacionaria
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
esp <- sum(primas%*%pi) 



