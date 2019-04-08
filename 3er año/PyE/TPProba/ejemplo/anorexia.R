#library(ggplot2)
  
base2 <- read.csv("/home/alumno/Desktop/TPProba/ejemplo/anorexia.data",sep="\t")

#0 dieta severa
#1 hiperactividad
#2 uso de laxantes
#3 uso de ropa holgada
attach(base2)
table(Signo)

#Gráfico de barras
barplot(table(Signo), xlab="Principal signo visible",ylab="Número de pacientes",main="Principal signo visible en pacientes con anorexia", names.arg = c("Dieta severa", "Hiperactividad", "Uso de laxantes", "Uso de ropa holgada"), ylim=c(0,20))

#Grafico de sectores
labels <- c("Dieta severa", "Hiperactividad", "Uso de laxantes", "Uso de ropa holgada")
#Agregar porcentajes a las etiquetas
pct <- round(table(Signo)*100/sum(table(Signo)))
labels <- paste(labels, pct) 
labels <- paste(labels,"%",sep="") 

pie(table(Signo),main="Principal signo visible en pacientes con anorexia", labels = labels)

#Diagrama de tallo y hojas
#edad <- (base$Edad)/10
#edad
#stem(edad)
stem(Edad)
potrillos<-c(129,119,132,123,112,113,95,104,93,108,95,117,128,127)
stem(potrillos)

#gráfico de bastones
frecVisitas <- table(Núm.de.visitas)
plot(frecVisitas)

#Histograma
#frecuencia absoluta
hist(Edad, xlab="Edad", ylab = "Frencuencia absoluta", main="Edad de los pacientes con anorexia", breaks = 8, xlim=c(10,35), ylim=c(0,20))
#frecuencia relativa
hist(Edad, xlab="Edad", main="Edad de los pacientes con anorexia", mids = 10, xlim=c(10,35), ylim=c(0,0.15), freq=FALSE)

#boxplot
#Número de visitas separando por sexo
boxplot(Núm.de.visitas~Sexo,data = base, xlab = "Sexo", ylab = "Número de visitas")
#Número de visitas separando por signo
boxplot(Núm.de.visitas~Signo,data = base, xlab = "Signo", ylab = "Número de visitas")
