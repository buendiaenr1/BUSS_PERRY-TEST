dat$Sexo <- factor(dat$Sexo,ordered=TRUE)

datos <- dat[-1]


require(psych)
require(likert)

g <- likert(datos)
plot(g, type="bar")

g <- likert(datos)
plot(g,type="heat",
           low.color = "white",
           high.color = "blue",
           text.color = "black",
           text.size = 4,
           wrap = 50)


#densidad
g <- likert(datos[,1:10])
plot(g,
     type="density",
           facet = TRUE,
           bw = 0.5)

g <- likert(datos[,11:20])
plot(g,
     type="density",
           facet = TRUE,
           bw = 0.5)

g <- likert(datos[,21:29])
plot(g,
     type="density",
           facet = TRUE,
           bw = 0.5)
