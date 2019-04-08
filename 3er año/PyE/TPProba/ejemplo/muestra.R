#Vector es el tipo básico de R
a <- 1
is.vector(a)
is.vector(4)

#R tiene tipado dinámico
v <- c(TRUE, TRUE, FALSE)
class(v)
v <- c(v, 0)
v
class(v)
v <- c(v, "probabilidad")
class(v)
v

#agregar sample

#Operaciones
v1 <- c(1,2,3,4)
v1 / 3
3 / v1
v2 <- c(2,3)
v1 * v2

#Listar los objetos
ls()

#Leo los datos

#Con setwd puede fijar el directorio de trabajo para evitar escribir la ruta todas las veces
#setwd("/home/")
datos <- read.csv('espirales.data',sep=",")

#Nos dice cosas útiles sobre los datos recién leídos
summary(datos)

#Leo los datos con header = FALSE
datos <- read.csv('espirales.data',header = FALSE,sep=",")

#Nos dice cosas útiles sobre los datos recién leídos
summary(datos)

nombres <- c("X", "Y", "CLASE")

#Leo los datos nombrando las variables
datos <- read.csv('espirales.data',col.names = nombres,header = FALSE,sep=",")

#Nos dice cosas útiles sobre los datos recién leídos
summary(datos)
class(datos)
dim(datos)

#Selecciono algunas filas y columnas
d <- datos[1:10,1:3]
dim(d)
#Selecciono una variable por su nombre
d$CLASE
#Selecciono una columna por su número de columna
d[,3]
#Filtrar según las clases
a <- d[d[,3]==1,]
#invertir matriz
a^-1
#trasponer
t(a)


#graficar. Grafico las primeras dos variables en el plano. La tercera variable la utilizo para definir el color
plot(datos[,-3],col=datos[,3]+2)

#Definir alguna función
funcion1 <- function(a){
  #seq(from=, to=, by=)
  v0 <- seq(from=0,to=a,by=2)
  #rep
  v1 <- rep(v0, 3)
  return (v1)
}

v1 <- c(1,2,3,4)
# mean es la función que calcula el promedio
mean(v1)
#Utilizar apply
v2 <- c(v1,v1)
dim(v2) <- c(2,4)
v2
#Calcular el promedio de cada columna
apply(X=v2, FUN=function(x) mean(x), MARGIN =2)
#Calcular el promedio de cada fila
apply(X=v2, FUN=function(x) mean(x), MARGIN =1)

#Calcular el producto de cada columna
apply(X=v2, FUN=prod, MARGIN =2)
#Calcular el producto de cada fila
apply(X=v2, FUN=prod, MARGIN =1)
