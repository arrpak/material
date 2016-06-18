
### Series temporales

# Para manejar información temporal hay que usar estructuras de datos que permitan 
# indexar datos por variables temporales. El paquete `zoo` es uno de los recomendados.

library(rvest)
library(reshape2)
library(ggplot2)
library(zoo)
library(tseries)

# De entre todos los paquetes anteriores, solo dos tienen que ver directamente con 
# series temporales, `zoo` y `tseries`; el segundo lo usaremos solo para descargar 
# datos de Yahoo dentro de la siguiente función:

foo  <- function( simbolo, final = Sys.time(), profundidad = 30 * 24 * 3600 ){
  precios <- get.hist.quote(instrument= simbolo, start = final - profundidad,
                            end = final, quote=c("AdjClose"),
                            provider="yahoo", origin="1970-01-01",
                            compression="d", retclass="zoo")
  colnames(precios) <- simbolo
  return(precios)
}







# Vamos a _scrapear_ una página de Yahoo para descargar los símbolos (abreviaturas 
# de los nombres de las acciones) de los valores de IBEX.

tmp <- read_html("http://finance.yahoo.com/q/cp?s=%5EIBEX+Components")
tmp <- html_nodes(tmp, "table")

tmp <- html_table(tmp[[9]])
simbolos <- tmp$Symbol

# Ahora podemos hacer una llamada a la función definida más arriba y usar `merge` (una 
# versión de `merge` que cruza por el índice temporal) para obtener una serie temporal 
# multidimensional:

ibex <- do.call(merge, sapply(simbolos, foo, simplify = F))

# Lo que hacemos a partir de este momento es hacer _clústering_ de series temporales 
# para asociar aquellos valores que tuvieron un comportamento similar durante el último 
# mes:

plot(ibex[,1])
plot(ibex[,2],add=T)

ibex.scaled <- scale(ibex)

ibex.df <- data.frame(ibex.scaled, fecha = index(ibex.scaled))
ibex.df <- melt(ibex.df, id.vars = "fecha")
ibex.df <- ibex.df[ order(ibex.df$fecha, ibex.df$variable), ]


mat <-t(ibex.scaled)

ibex.df$cluster <- kmeans(data.frame(t(ibex.scaled)), 4)$cluster #clustering

ggplot(ibex.df, aes(x=fecha, y=value, group=variable)) + 
  geom_line() + facet_wrap(~cluster)

# La función `plot` sabe cómo representar (o, tal vez mejor dicho, se adapta para representar) 
# series temporales:

head(ibex)
plot(ibex[,1:4])
plot(ibex[,1:4], plot.type = "single")


r <- get.hist.quote(instrument = "^dji", start= "1899-12-30", end =Sys.time(),
                   quote = c("Open", "High", "Low", "Close"),
                   provider = c("yahoo", "oanda"), method = NULL,
                   origin = "1899-12-30", compression = "d",
                   retclass = c("zoo", "its", "ts"), quiet = FALSE, drop = FALSE) 

r
plot(r)


# Hay más funciones propias de series temporales que, por supuesto, permite hacer `zoo`. 
# Por ejemplo, diferencias entre los valores de un día y el siguiente:

plot(diff(ibex[,1:4]))

# O aplicar una función `por ventanas`:
plot(rollapply(ibex, 10, sd)[,1:4])

# Y contiene operaciones de selección específicas:
start(ibex)
end(ibex)
window(ibex, start = Sys.Date() - 7, end = Sys.Date())

# La librería 

library(xts)

# contiene funciones adicionales para la manipulación de series temporales que permiten 
# hacer cosas como:

ibex.xts <- as.xts(ibex)

ibex.xts["2016-05"]         # solo mayo
first(ibex.xts, "1 week")
last(ibex.xts, "1 week")

##### Ejercicio
# Repasar [este documento](https://cran.r-project.org/web/packages/xts/vignettes/xts.pdf) 
# y practicar las funciones que describe.

periodicity(ibex.xts)

endpoints(ibex.xts,on='months')

period.apply(ibex.xts[,4],INDEX = endpoints(ibex.xts,'weeks'),FUN=mean)

### Más sobre proceso de texto

# En primer lugar, vamos a leer el Quijote completo:

quijote <- readLines("http://www.gutenberg.org/cache/epub/2000/pg2000.txt")

##### Ejercicio
# Encuentra las palabras más frecuentes en la obra. Ten en cuenta mayúsculas, etc. 
# Además, que hay texto en inglés al principio y al final, etc.

##### Ejercicio
# Encuentra los bigramas del Quijote, es decir, parejas de palabras consecutivas (y 
# no separadas por puntos, comas u otros separadores). Los bigramas y los trigramas 
# son fundamentales para el análisis de texto natural.


head(quijote,230)
library(tm)
getTokenizers()
