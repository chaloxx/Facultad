base <- read.csv("/home/alumno/Desktop/TPProba/base8.txt",sep="\t")
attach(base)
summary(especie)
n <- length(ID)

#Histrograma para altura
tablaFrecAbs <- table(altura)
tablaFrecRel <- tablaFrecAbs/n
tablaPorc <- tablaFrecRel * 100
hist(altura, xlab = "Altura", ylab = "Frecuencia Relativa",main = "Altura de los àrboles", freq = FALSE, col = "red")
#El porcentaje mayor de àrboles tiene una altura de entre 10 y 15
#El promedio en altura es de 14.85 y la mediana es de 15

#Histrograma para el diámetro
stem(diametro,width = 90)
#El promedio para el diametro es de 40.82 y la mediana es de 33


#Pie para la inclinacion
install.packages("qcc")
library(qcc)
inclinación2 <- inclinación
inclinación2[inclinación>0 & inclinación<=10] <- 1
inclinación2[inclinación>10] <- 2
tablaFrecAbsInclinacion <- table(inclinación2)
labels <- c("Sin inclinación", "Con poca inclinación", "Bastante inclinación")
pareto.chart(tablaFrecAbsInclinacion,name=labels, xlab="Tipos de inclinación", ylab ="Frecuencia")
#La moda es tener 0 inclinación, su frecuencia relativa es de 0,69 aproximadamente.

#Grafico de barras para las especies
nombresEspecies = c("Ficus", "Ceibo", "Acacia", "Fresno", "Jacarandá", "Palo borracho", "Casualina", "Eucalipto", "Álamo")

barplot(sort(table(especie)), xlab="Especies", ylab="Número de árboles", names.arg = nombresEspecies, ylim=c(0,80))
FrecRelEspecies <- sort(table(especie))/n
#Observamos que la moda es el Álamo y su frecuencia relativa es de 0.20 aproximadamente.


#Torta para el origen

labels = c("Exótico","Nativo/Autóctono")
pct <- round(table(origen)*100/sum(table(origen)))
labels <- paste(labels, pct) 
labels <- paste(labels,"%",sep="") 
pie(table(origen),main="Distribucion de los arboles segun su origen", labels = labels)
#La moda es exótica y su frecuencia relativa es de 0,72

#Gráfico de bastones para la cantidad de brotes

plot(table(brotes),xlab="Cantidad de brotes",ylab="Frecuencia")
#El promedio en brotes de aproximadamente de 3.53 y la mediana es de 3
arbolesSinBrote <- especie[brotes == 0]


#Hacemos una comparación de la altura promedio entre las distintas especies                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
boxplot(altura~especie)





