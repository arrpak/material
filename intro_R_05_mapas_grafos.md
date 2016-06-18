---
title: "Mapas, grafos, series temporales, etc."
author: "Carlos J. Gil Bellosta"
date: "May 26, 2016"
output: html_document
---



## Mapas con ggmap

El paquete `ggmap` está pensado para representar mapas e información sobre mapas con una sintaxis similar a la de `ggplot2`. El paquete contiene tres tipos de funciones:
* Para descargar mapas de diversas fuentes
* Para representar capas (o _geoms_) sobre dichos mapas.
* Para utilizar recursos geográficos, como los de geolocalización, disponibles en diversas APIs.

Por defecto, se utilizan los servicios de Google Maps, que están sujetos a limitaciones de uso. Pero es posible consultar otros.



```r
library(ggmap)
```

En primer lugar, vamos a descargar las coordenadas correspondientes a una dirección.


```r
unizar <- geocode('Universidad de Zaragoza, Zaragoza, España', source = "google")
unizar         # coordenadas de la universidad de Zaragoza
```

Dado un _centro_, es posible descargar un mapa de la zona.


```r
map.unizar <- get_map(location = as.numeric(unizar),
                      color = "color",
                      maptype = "roadmap",
                      scale = 2,
                      zoom = 16)
```

#### Ejercicio
Prueba con otras ubicaciones, otros zums, otros tipos de mapas, etc. Pista: `?get_map`.

Sobre un mapa es posible representar diversas capas como, por ejemplo, de puntos:


```r
ggmap(map.unizar) + geom_point(aes(x = lon, y = lat),
                               data = unizar, colour = 'red',
                               size = 4)
```


#### Ejercicio
Pinta las gasolineras en el mapa de España. 

Además de puntos, se pueden representar, por ejemplo, rutas:


```r
mapa <- get_map("Madrid", source = "stamen", maptype = "toner", zoom = 12)
ruta <- route(from = "Puerta del Sol, Madrid", to = "Plaza de Castilla, Madrid")
ggmap(mapa) + 
  geom_path(aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
            colour = "red", size = 2, data = ruta)
```

Para averiguar más sobre el paquete, puedes consultar [este artículo](https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf).


## Mapas y shapefiles

Los `shapefiles` son estructuras de datos que combinan información geográfica (típicamente, perímetros de regiones) con información estadística relativa a dichos elementos. Un _shapefile_ no es un fichero sino, típicamente, un grupo de ellos que comparten nombre, tienen diversas extensiones y que se leen conjuntamente. 

En R, los _shapefiles_ pueden leerse con el paquete _rgdal_, que los convierte en objetos S4. Los objetos S4 son unas extensiones de R para la programación orientada a objetos, que es muy conveniente para procesar información de tipo geográfico. Pero que exigen el uso de una (otra) sintaxis particular.


```r
library(rgdal)
sport <- readOGR(dsn = "data/london_sport", "london_sport")
class(sport)
```

Un objeto de la clase S4 tiene _slots_, que son, abusando del lenguaje, las _variables_ del objeto.


```r
slotNames(sport)
```

Típicamente, la más importante es `data`.


```r
sport@data
```

En efecto, un objeto de la clase `SpatialPolygonsDataFrame` es una tabla (_dataframe_) más una lista de _polígonos_, asociados a ella y que permite representarlos en un mapa. Un `SpatialPolygonsDataFrame` permite realizar operaciones de filtrado, etc. similares a las que ya sabemos hacer con tablas.


```r
sport@data[sport$Partic_Per < 15, ]

plot(sport[sport$Partic_Per > 25, ]) 

plot(sport)
plot(sport[sport$Partic_Per > 25, ], col = "blue", add = TRUE)

plot(sport, col = sport$Pop_2001)

summary(sport)
```

#### Ejercicio
Añade una columna al objeto `sport` y represéntala gráficamente con la intensidad según el valor asociado. Pista: visita 
[esta página](http://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf) y busca el código que acompaña al mapa de interés.

#### Ejercicio (avanzado)
Descargar los _shapefiles_ de provincias del INE, obtén datos de algún tipo de estadistica por provincias (¿paro? ¿población?), crea un _SpatialPolygonsDataFrame_ con esa información y represéntala gráficamente.

#### Ejercicio (avanzado) 
Trata de combinar _shapefiles_ con `ggmap`.


## Grafos y redes sociales

En esta sección vamos a explorar el paquete `igraph` para leer, procesar, extraer información y representar gráficamente datos que tienen estructura de red. Para ello usaremos información disponible en internet de `güifi.net`, un proyecto para crear redes comunitarias de internet vía wifi.

En primer lugar, cargaremos los paquetes necesarios:


```r
library(XML)
library(plyr)
library(igraph)
library(ggmap)
library(popgraph)
```

Luego bajaremos y procesaremos el fichero .xml que guarda información de la subred de la zona de Barcelona.


```r
tmp <- readLines("https://guifi.net/en/guifi/cnml/2435/detail")
tmp <- xmlParse(tmp)
```

Con [XPath](https://es.wikipedia.org/wiki/XPath) podemos extraer la información de los nodos para después poder extraer sus atributos.


```r
nodos <- xpathApply(tmp, "//*/node")

# extraemos los atributos de los nodos
lista.nodos <- rbind.fill(lapply(
  nodos, function(x) data.frame(t(xmlAttrs(x)), 
                                stringsAsFactors = FALSE)))
```

De la misma manera, podemos extraer los enlaces.


```r
lista.links <- lapply(nodos, function(x){
  from <- xmlAttrs(x)["id"]
  to   <- xpathApply(x, ".//link", xmlAttrs)
  
  if (length(to) == 0)
    return(NULL)
  
  to <- sapply(to, function(x) x["linked_node_id"])
  data.frame(from = from, to = to, stringsAsFactors = FALSE)
})

# lo anterior es una lista de tablas; ahora las apilamos todas
lista.links <- do.call(rbind, lista.links)
```

#### Ejercicio
Reescribe lo anterior usando `ldply` en lugar de `do.call`.

Finalmente, filtramos los nodos _redundantes_ (enlaces cuyos nodos no están en la lista de nodos, etc.) y creamos el grafo.


```r
lista.links <- lista.links[lista.links$from %in% lista.nodos$id,]
lista.links <- lista.links[lista.links$to   %in% lista.nodos$id,]
lista.links <- lista.links[lista.links$to != lista.links$from,]

g <- graph.data.frame(lista.links, directed = F, lista.nodos)
g.working <- subgraph(g, V(g)$status %in% c("Working"))
```

Entonces podemos representarlo gráficamente usando un _layout_: 


```r
my_layout <- layout.fruchterman.reingold(g.working)
plot(g.working, layout = my_layout, vertex.label = NA, vertex.size = 0.3)
```

#### Ejercicio
Explora otros _layouts_ y mira a ver qué aspecto le dan a la red.

Podemos extraer la mayor componente conexa:


```r
tmp <- clusters(g.working)
g.wc <- subgraph(g.working, kk$membership == which.max(tmp$csize))
```

#### Ejercicio
Explora el objeto `tmp`.

Podemos enriquecer el gráfico con información calculada a partir del grafo, como el _betweenness_:


```r
g.wc <- set.edge.attribute(g.wc, name = "betweenness", E(g.wc), 
                           edge.betweenness(g.wc))
g.wc <- set.vertex.attribute(g.wc, name = "btw", 
                             V(g.wc), betweenness(g.wc))

my_layout <- layout.fruchterman.reingold(g.wc)
plot(g.wc, layout = my_layout, vertex.label = NA, 
     vertex.size = (1 + V(g.wc)$btw) / 3000)
```

En ocasiones, como esta, es útil crear un _layout_ a mano:


```r
geom.layout <- cbind(as.numeric(V(g.wc)$lon),
                     as.numeric(V(g.wc)$lat))
plot(g.wc, layout = geom.layout, vertex.label = NA, 
     vertex.size = (1 + V(g.wc)$btw) / 3000)
```

Para terminar, podemos incluir el mapa base por debajo del grafo:


```r
map.bcn <- get_map("Barcelona", maptype="satellite", zoom = 12)
tmp <- as.popgraph(g.wc)
tmp <- set.vertex.attribute(tmp, name = "Longitude", V(tmp), 
                            value = as.numeric(V(tmp)$lon))
tmp <- set.vertex.attribute(tmp, name = "Latitude", V(tmp), 
                            value = as.numeric(V(tmp)$lat))
p <- ggmap(map.bcn) 
p <- p + geom_edgeset(aes(x = Longitude, y = Latitude), tmp, color="white" )
p <- p + geom_nodeset(aes(x = Longitude, y = Latitude, size = btw), tmp)
p
```

## Series temporales

Para manejar información temporal hay que usar estructuras de datos que permitan indexar datos por variables temporales. El paquete `zoo` es uno de los recomendados.


```r
library(rvest)
library(reshape2)
library(ggplot2)
library(zoo)
library(tseries)
```

De entre todos los paquetes anteriores, solo dos tienen que ver directamente con series temporales, `zoo` y `tseries`; el segundo lo usaremos solo para descargar datos de Yahoo dentro de la siguiente función:


```r
foo  <- function( simbolo, final = Sys.time(), profundidad = 30 * 24 * 3600 ){
  precios <- get.hist.quote(instrument= simbolo, start = final - profundidad,
                            end = final, quote=c("AdjClose"),
                            provider="yahoo", origin="1970-01-01",
                            compression="d", retclass="zoo")
  colnames(precios) <- simbolo
  return(precios)
}
```

Vamos a _scrapear_ una página de Yahoo para descargar los símbolos (abreviaturas de los nombres de las acciones) de los valores de IBEX.


```r
tmp <- read_html("http://finance.yahoo.com/q/cp?s=%5EIBEX+Components")
tmp <- html_nodes(tmp, "table")

tmp <- html_table(tmp[[9]])
simbolos <- tmp$Symbol
```

Ahora podemos hacer una llamada a la función definida más arriba y usar `merge` (una versión de `merge` que cruza por el índice temporal) para obtener una serie temporal multidimensional:


```r
ibex <- do.call(merge, sapply(simbolos, foo, simplify = F))
```

Lo que hacemos a partir de este momento es hacer _clústering_ de series temporales para asociar aquellos valores que tuvieron un comportamento similar durante el último mes:


```r
ibex.scaled <- scale(ibex)

ibex.df <- data.frame(ibex.scaled, fecha = index(ibex.scaled))
ibex.df <- melt(ibex.df, id.vars = "fecha")
ibex.df <- ibex.df[ order(ibex.df$fecha, ibex.df$variable), ]
ibex.df$cluster <- kmeans(data.frame(t(ibex.scaled)), 4)$cluster

ggplot(ibex.df, aes(x=fecha, y=value, group=variable)) + 
  geom_line() + facet_wrap(~cluster)
```

La función `plot` sabe cómo representar (o, tal vez mejor dicho, se adapta para representar) series temporales:


```r
plot(ibex[,1:4])
plot(ibex[,1:4], plot.type = "single")
```

Hay más funciones propias de series temporales que, por supuesto, permite hacer `zoo`. Por ejemplo, diferencias entre los valores de un día y el siguiente:


```r
plot(diff(ibex[,1:4]))
```

O aplicar una función `por ventanas`:

```r
plot(rollapply(ibex, 10, sd)[,1:4])
```

Y contiene operaciones de selección específicas:

```r
start(ibex)
end(ibex)
window(ibex, start = Sys.Date() - 7, end = Sys.Date())
```

La librería 


```r
library(xts)
```

contiene funciones adicionales para la manipulación de series temporales que permiten hacer cosas como:


```r
ibex.xts <- as.xts(ibex)

ibex.xts["2016-01"]         # solo enero
first(ibex.xts, "1 week")
last(ibex.xts, "1 week")
```

#### Ejercicio
Repasar [este documento](https://cran.r-project.org/web/packages/xts/vignettes/xts.pdf) y practicar las funciones que describe.


## Más sobre proceso de texto

En primer lugar, vamos a leer el Quijote completo:


```r
quijote <- readLines("http://www.gutenberg.org/cache/epub/2000/pg2000.txt")
```

#### Ejercicio
Encuentra las palabras más frecuentes en la obra. Ten en cuenta mayúsculas, etc. Además, que hay texto en inglés al principio y al final, etc.

#### Ejercicio
Encuentra los bigramas del Quijote, es decir, parejas de palabras consecutivas (y no separadas por puntos, comas u otros separadores). Los bigramas y los trigramas son fundamentales para el análisis de texto natural.
