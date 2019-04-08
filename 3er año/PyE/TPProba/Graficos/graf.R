#Nuestra  base
base <-read.csv("/home/alumno/Desktop/TPProba/base8.txt",sep="\t")
attach(base)
n = length(ID)

#Para la altura usaremos un histograma a fin de poder apreciar mejor la dispersiòn
tablaFrecAbs=table(altura)
tablaFrecRel=tablaFrecAbs/n
hist(tablaFrecAbs, xlab="altura", ylab = "Frecuencia Relativa", xlim=c(0,40), ylim=c(0,20))


#Para el diàmetro usaremos
