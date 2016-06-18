library(plyr)


#mi.iris <- iris
#mi.iris$cutoff <- length(mi.iris$Species)/2
#mi.iris$indices <- sample(1:length(mi.iris$Species),length(mi.iris$Species))
#mi.iris <- mi.iris[mi.iris$indices < mi.iris$cutoff,]


foo <- function(datos)
{
  datos$cutoff <-  length(datos$Species)/2
  datos$indices <- sample(1:length(datos$Species),length(datos$Species))
  datos    <- datos[datos$indices <= datos$cutoff,]
}


mi.iris <- iris

#      mi.iris2 <- ddply(mi.iris,.(mi.iris$Species),foo)

mi.iris2 <- ddply(mi.iris,.(Species),function (datos)
{
  datos$cutoff <-  length(datos$Species)/2
  datos$indices <- sample(1:length(datos$Species),length(datos$Species))
  datos    <- datos[datos$indices <= datos$cutoff,]
})

foomi.iris <- foo(mi.iris)

tapply(mi.iris2$Sepal.Length, mi.iris2$Species, length)
tapply(foomi.iris$Sepal.Length, foomi.iris$Species, length)

