install.packages("qcc",dependencies=TRUE,repos='http://cran.rstudio.com/')
library(qcc)
base <- read.csv("/home/winter/Escritorio/TPProba/base8.txt",sep="\t")
attach(base)

#Histrograma para altura
hist(altura,
     main = "Gráfico 3: Altura de los árboles",
     xlab = "Altura (en metros)",
     ylab = "Frecuencia absoluta",
     col = "lightblue",
     ylim = c(0,100))

#Grafico de tallo y hoja para el diámetro
stem(diametro,
     width = 100)

#Grafico de Pareto para la inclinacion
inclinación2 <- inclinación
inclinación2[inclinación>0 & inclinación<=10] <- 1
inclinación2[inclinación>10] <- 2
pareto.chart(table(inclinación2),
             main = "Gráfico 5: Inclinación de los árboles",
             xlab = "Inclinación",
             ylab = "Frecuencia absoluta",
             ylab2 = "Porcentaje acumulado",
             name = c("Nula", "Poca", "Mucha"),
             las = 1)

#Grafico de barras para las especies
nombresEspecies = c("Álamo","Eucalipto","Casuarina",
                    "Palo borracho","Jacarandá","Fresno",
                    "Acacia","Ceibo","Ficus")
barplot(sort(table(especie), decreasing = TRUE),
        main = "Figura 2: Cantidad de árboles por especie",
        xlab = "Frecuencia absoluta",
        xlim = c(0,80),
        names.arg = FALSE,
        horiz = TRUE,
        las = 2,
        col = "khaki3")

#gráfico de Torta para el origen
pct <- round(table(origen)*100/sum(table(origen)))
labels <- c("Exótico","Nativo/Autóctono")
labels <- paste(labels, pct,sep="\n") 
labels <- paste(labels,"%")
pie(table(origen),
    main = "Figura 5: Distribución de los árboles según su origen",
    labels = labels,
    col = c("tomato","yellowgreen"))

#Gráfico de bastones para la cantidad de brotes
plot(table(brotes),
     main = "Figura 7: Cantidad de brotes de los arboles",
     xlab = "Brotes",
     ylab = "Frecuencia absoluta")

#Hacemos una comparación de la altura entre las distintas especies                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
boxplot(altura~especie,
        main = "Figura 8: Altura de los árboles segun su especie",
        xlab = "Especie",
        ylab = "Altura (en metros)")

