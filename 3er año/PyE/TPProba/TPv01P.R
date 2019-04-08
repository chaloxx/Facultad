base <- read.csv("/home/pablo/Escritorio/base8.txt",sep="\t")

#base <- read.csv("base8.txt",sep="\t")
attach(base)



n <- length(ID)

#Histrograma para altura
tablaFrecAbs <- table(altura)
tablaFrecRel <- tablaFrecAbs/n
tablaPorc <- tablaFrecRel * 100
hist(altura, xlab = "Altura", ylab = "Frecuencia Relativa",main = "Figura 1: Distribucion en la altura", freq = FALSE, col = "red",ylim = c(0,0.06))
quantile(altura)#La mayor cantidad de observaciones se encuentran en el intervalo [10,20]
mean(altura);median(altura)#El promedio en altura es de 14.85 y la mediana es de 15

#Diagrama de tallo y hoja para el diámetro
stem(diametro,width = 90)
quantile(diametro)#La mayor cantidad de observaciones se encuentran en el intervalo [22,53]
mean(diametro);median(diametro)#El promedio para el diametro es de 40.82 y la mediana es de 33
#Por la tanto la distribucion es asímetrica positiva
boxplot(diametro)#A trávez del boxplot deducimos que los valores extremos son los mayores a 100 cm


#Pareto para la inclinacion
#install.packages("qcc")
library(qcc)
inclinacion2 <- inclinación
inclinacion2[inclinación == 0] <- "0°"
inclinacion2[inclinación>0 & inclinación<=10] <- "0° a 10°"
inclinacion2[inclinación>10] <- "Mayor a 10°"
tablaFrecAbsInclinacion <- table(inclinacion2)
labels <- c("0°", "0° a 10°", "Mayor a 10°")
pareto.chart(tablaFrecAbsInclinacion, xlab="Inclinación", ylab ="Frecuencia Absoluta",ylab2 = "Porcentaje acumulado", main = "Figura 4: Distribucion en la inclinacion", las = 1)
#Con el gráfico de pareto observamos que la moda es no tener inclinación
#Podemos concluir entonces que una gran parte de los árboles crece perpendicular al suelo.
#Será porque no tienen problemas para recibir la luz del sol?


#Grafico de barras para las especies
nombresEspecies = c("Ficus", "Ceibo", "Acacia", "Fresno", "Jacarandá", "Palo borracho", "Casualina", "Eucalipto", "Álamo")
barplot(sort(table(especie),decreasing = TRUE), xlab="Especies", ylab="Número de árboles", names.arg = nombresEspecies, ylim=c(0,80))
FrecRelEspecies <- sort(table(especie))/n
#Observamos que la moda es el Álamo y su frecuencia relativa es de 0.20 aproximadamente.


#Torta para el origen
labels = c("Exotico","Nativo/Autoctono")
pct <- round(table(origen)*100/sum(table(origen)))
labels <- paste(labels, pct) 
labels <- paste(labels,"%",sep="") 
pie(table(origen),main="Distribucion de los arboles segun su origen", labels = labels)
#La moda es exotica y su frecuencia relativa es de 0,72

#Gráfico de bastones para la cantidad de brotes

plot(table(brotes),xlab="Cantidad de brotes",ylab="Frecuencia")
#El promedio en brotes de aproximadamente de 3.53 y la mediana es de 3
#La distribución es asimétrica positiva


#Hacemos una comparacion de la altura  entre las distintas especies                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
boxplot(altura~especie, xlab = "Especies", ylab = "Altura (metros)")


#Comandos extras

Inc<-ordered(inclinacion2,levels=c("Nada","Poco","Mucho"))
frecabs<-table(Inc)
frecrel<-prop.table(table(Inc))
porc<- frecrel*100
tabla<-cbind(frecabs,frecrel,porc)

plot(origen~especie)
