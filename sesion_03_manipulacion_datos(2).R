#############################################################################
# Ciencia de datos - R - Parte 03: manipulación avanzada de datos con R
# cgb@datanalytics.com, 2016-05-21
#
# El objetivo de esta sesión es recorrer aprender a manipular datos usando dos
# paquetes importantes de R: reshape2 y plyr
#############################################################################

#############################################################################
# reshape2
#############################################################################

# Instalación:
install.packages("reshape2")
install.packages("plyr")

# Nota: también puedes usar los menús de RStudio para instalar paquetes (Tools...)

# Carga:
library(reshape2)
library(plyr)

#----------------------------------------------------------------------------
# formato largo (melted)
#----------------------------------------------------------------------------

pob.aragon.2014 <- read.table("data/pob_aragon_2014.csv", header = T, sep = "\t")
pob.aragon.2014

melt(pob.aragon.2014)          # mismos datos en otro formato... ¡formato largo!

# Ejercicio: pasa el tiempo que consideres necesario para entender muy bien:
#   - cómo se ha transformado pob.aragon.2014
#   - que la información contenida en ambos conjuntos de datos es la misma

pob.aragon <- read.table("data/pob_aragon.csv", header = T, sep = "\t")
pob.aragon

melt(pob.aragon)               # ¡horrible!
melt(pob.aragon, id.vars = c("Provincia", "Periodo"))   # Ahora sí

# Ejercicio: ¿qué pasa si alteras el orden de provincia y periodo?

pob.aragon.long <- melt(pob.aragon, id.vars = c("Provincia", "Periodo")) 

# Una pequeña digresión:
arrange(pob.aragon.long, Periodo, Provincia)     # ¿te gusta más ordenar así?

# Nota: la función arrange está en el paquete plyr...

# Ejercicio: busca en ?arrange cómo ordenar descendentemente

# Ejercicio: toma el conjunto de datos airquality y disponlo en formato largo

# Ejercicio: calcula el valor mediano (median) de las variables de long.airquality

#----------------------------------------------------------------------------
# Formato ancho (cast)
#----------------------------------------------------------------------------

pob.aragon.2014.largo <- melt(pob.aragon.2014)
pob.aragon.2014.largo

# a partir del formato largo se puede pasar a distintos tipos de formatos anchos:

dcast(pob.aragon.2014.largo, Provincia ~ variable)
dcast(pob.aragon.2014.largo, variable ~ Provincia)

# Agregaciones

iris.long <- melt(iris)
head(iris.long)

dcast(iris.long, Species ~ variable)    

# Ejercicio: ¿qué ha pasado?

dcast(iris.long, Species ~ variable, fun.aggregate = mean) 

dcast(iris.long, Species ~ variable, value.var = "value", fun.aggregate = mean)  

# Nota: generalmente, no hay que especificar "value.var": dcast la adivina. Pero a veces se
#   equivoca, por lo que...


paro <- read.table("data/paro.csv", header = T, sep = "\t")

# vamos a arreglar un poco los datos (los detalles, más adelante)
paro$Periodo <- gsub("IV",  "4", paro$Periodo)
paro$Periodo <- gsub("III", "3", paro$Periodo)
paro$Periodo <- gsub("II",  "2", paro$Periodo)
paro$Periodo <- gsub("I",   "1", paro$Periodo)

paro$Situation <- as.character(paro$Situation)

paro$Situation[paro$Situation == "Active population"]   <- "active"
paro$Situation[paro$Situation == "Inactive persons"]    <- "inactive"
paro$Situation[paro$Situation == "Unemployed persons"]  <- "unemployed"
paro$Situation[paro$Situation == "Employed persons"]    <- "employed"
paro$Situation[paro$Situation == "Parados que buscan primer empleo"]    <- "never_employed"

paro$Situation <- factor(paro$Situation)

# paro está en formato largo, pero...
paro.alt <- dcast(paro, Gender + Provinces + Periodo ~ Situation)

# Ejercicio: añade a paro.alt una columna adicional con la tasa de paro (desempleados entre
#   población activa)

# Nota: este ejercicio demuestra que en ocasiones es bueno crear un determinado tipo de formato
#   largo para crear nuevas variables fácilmente.

# Ejercicio: agrega los datos del paro para toda España usando dcast y fun.aggregate = sum.
#   Pista: si ignoras la provincia en dcast se producen "duplicados"

# Ejercicio: identifica las provincias y periodos en los que la tasa de paro masculina es
#   mayor que la femenina (nota: la tasa de paro es "unemployed" dividido por "active")


#----------------------------------------------------------------------------
# plyr: procesamiento de tablas por trozos
#----------------------------------------------------------------------------

# la expresión fundamental:

res <- ddply(paro, .(Gender, Periodo, Situation), summarize, total = sum(value))

# elementos de la expresión anterior:
# ddply: transforma una tabla en otra tabla
# paro: un dataframe
# .(...): variables de la tabla de entrada por las que se parte 
# summarize: cualquier función que opera sobre tablas
# total = ...: argumentos de la función

# Ejercicio: pon airquality en formato largo y saca la media y la mediana de cada variable por mes

# otras funciones que se pueden usar en ddply:
foo <- function(x) lm(Temp ~ Solar.R, data = x)$coefficients
ddply(airquality, .(Month), foo)

# En general, insisto, la función puede ser cualquiera que admita como argumento una tabla
# Los demás argumentos de la función (arbitraria) se pasan a través de ddply (detrás de la llamada a
#   la función)

# variantes de la fórmula anterior: dlply
res <- dlply(airquality, .(Month), function(x) lm(Temp ~ Solar.R, data = x))  # una lista!
lapply(res, coefficients)
ldply(res, coefficients)

# existen también llply, laply, alply... e incluso d_ply

# ejercicio: completa la función siguiente y úsala para guardar un gráfico de la relación entre la temperatura
# y la irradiación solar en cada mes

foo <- function(x){
  nombre.fichero <- paste0(unique(x$Month), ".png")
  png(nombre.fichero)
    plot(x$Solar.R, x$Temp, main = "...", xlab = "...", ylab = "...")
    abline(lm(Temp ~ Solar.R, data = x), col = "red")     
  dev.off()
}


# transformaciones por trozos

tasa.paro <- dcast(paro, Gender + Provinces + Periodo ~ Situation)
tasa.paro <- transform(tasa.paro, tasa.paro = unemployed / active)
tasa.paro <- tasa.paro[, c("Gender", "Provinces", "Periodo", "tasa.paro")]

# Para seleccionar el perido de mayor tasa de paro en cada provincia y sexo, con plyr,
tmp <- ddply(tasa.paro, .(Gender, Provinces), transform, rank = rank(-tasa.paro, ties = "random"))
res <- tmp[tmp$rank == 1,]

# Ejercicio: selecciona en cada provincia el periodo en el que fue máximo el número total (hombres + mujeres) de parados


# Un ejemplo de regresiones por trozos y asignar el valor predicho.
# Usamos data de http://www.unt.edu/rss/class/Jon/R_SC/Module9/lmm.data.txt
dat <- read.table("data/lmm_data.txt", header = T, sep = ",")

dat.preds <- ddply(dat, .(school), transform, 
                   pred = predict(lm(extro ~ open + agree + social + class)))


# Alternativamente:

foo <- function(x){
  modelo <- lm(extro ~ open + agree + social + class, data = x)
  res <- x
  res$preds <- predict(modelo, new.data = x)
  res
}

dat.preds <- ddply(dat, .(school), function(x) foo(x))
dat.preds <- ddply(dat, .(school), foo)