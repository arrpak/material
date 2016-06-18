
## author: "Carlos J. Gil Bellosta"
## date: "2016-05-21"

# knitr::opts_chunk$set(echo = TRUE)

### Gráficos avanzados con ggplot2

library(ggplot2)
library(reshape2)

# Vamos a cargar y limpiar la tabla de datos del paro. De nuevo, los detalles se desarrollarán 
# más adelante; por el momento, pueden obviarse.

paro <- read.table("/home/dsc/Downloads/data/paro.csv", header = T, sep = "\t")

# vamos a arreglar un poco los datos (los detalles, más adelante)
paro$Periodo <- gsub("QIV",  "-12-31", paro$Periodo)
paro$Periodo <- gsub("QIII", "-09-30", paro$Periodo)
paro$Periodo <- gsub("QII",  "-06-30", paro$Periodo)
paro$Periodo <- gsub("QI",   "-03-31", paro$Periodo)

paro$Periodo <- as.Date(paro$Periodo)

paro$Situation <- as.character(paro$Situation)

paro$Situation[paro$Situation == "Active population"]   <- "active"
paro$Situation[paro$Situation == "Inactive persons"]    <- "inactive"
paro$Situation[paro$Situation == "Unemployed persons"]  <- "unemployed"
paro$Situation[paro$Situation == "Employed persons"]    <- "employed"
paro$Situation[paro$Situation == "Parados que buscan primer empleo"]    <- "never_employed"

paro$Situation <- factor(paro$Situation)

# Vamos a calcular la tasa de paro utilizando funciones ya conocidas: 


paro <- dcast(paro, Gender + Provinces + Periodo ~ Situation)
paro <- transform(paro, tasa.paro = 100 * unemployed / active)

# Y ahora vamos a representar la evolución de la tasa de paro por provincias así:

ggplot(paro, aes(x = Periodo, y = tasa.paro, col = Gender)) +  #col es color
  geom_point() + geom_smooth() + #capas
  facet_wrap(~ Provinces)    #facetas
#solo vale para dataframes


# En la expresión anterior coexisten varios elementos (y una sintaxis particular):
# * Estéticas, dentro de `aes`, que indican una correspondencia entre variables existentes 
# en los datos y _estéticas_ o _significantes gráficos_, es decir, atributos representables 
# gráficamente (p.e., longitudes tanto en el eje x como en el y, tamaños, transparencias, 
# formas, etc.)
# * Geometrías o capas, que reordenan las estéticas de una determinada forma; nótese 
# como en el ejemplo anterior hemos superpuesto dos geometrías (y de ahí el nombre 
# alternativo de capas).
# * Facetas, que dividen el lienzo en sublienzos dentro de los cuales se representan 
# las geometrías. Es una implementación de la técnica de los pequeños múltiplos.

# La sintaxis de `ggplot2` es un tanto extraña: los distintos elementos de un gráfico 
# se van sumando. De hecho, `ggplot2` es la implementación de la llamada gramática 
# de los gráficos, una propuesta para elaborar un lenguaje con el que combinar elementos 
# gráficos para crear visualizaciones complejas de la misma manera que el lenguaje 
# hablado, con una cierta sintaxis, compone frases complejas utilizando conceptos o 
# palabras elementales.

# `ggplot2` es un paquete muy amplio y en extensión. Pero tiene una [documentación](http://docs.ggplot2.org/current/) 
# estupenda. Cuando quieras crear un gráfico, visita ese enlace para consultar las 
# estéticas disponibles, su sintaxis, etc.

##### Ejercicio

# Sírvete de la documentación arriba enlazada para modificar el gráfico

tmp <- paro[paro$Provinces %in% c("50 Zaragoza", "22 Huesca", "44 Teruel"),]
ggplot(tmp, aes(x = Periodo, y = tasa.paro)) +
  geom_point() + geom_smooth() + facet_grid(Provinces~Gender)

ggplot(tmp, aes(x = Periodo, y = tasa.paro)) +
  geom_point() +  facet_grid(Provinces~Gender)

ggplot(tmp, aes(x = Periodo, y = tasa.paro)) +
  geom_point() + geom_smooth() + facet_wrap(Provinces~Gender)

ggplot(tmp, aes(x = Periodo, y = tasa.paro)) +
  geom_smooth() + facet_wrap(Provinces~Gender)

ggplot(tmp, aes(x = Periodo, y = tasa.paro)) +
  geom_smooth() + facet_wrap(Gender~Provinces)


ggplot(tmp, aes(x = Periodo, y = tasa.paro)) +
  geom_smooth() + facet_wrap(~Provinces)

ggplot(tmp, aes(x = Periodo, y = tasa.paro)) +
  geom_smooth() + facet_wrap(~Gender)


ggplot(tmp, aes(x = Periodo, y = tasa.paro)) +
  geom_smooth() + facet_grid(~Provinces)

ggplot(tmp, aes(x = Periodo, y = tasa.paro)) +
  geom_smooth() + facet_grid(~Gender)



# usando otro tipo de facetas. 

##### Ejercicio
# Haz un diagrama de cajas de las temperaturas en NY por mes (sin facetas). Recuerda 
# convertir previamente el mes a factor.



ggplot(airquality, aes(x = as.factor(Month), y = Temp)) + 
  geom_boxplot(color = "red") 



ggplot(airquality, aes(x = factor(Month), y = Temp)) + labs(title="Temperatura") + xlab("Meses") +
  geom_boxplot(color = "green" , outlier.color = "blue", fill="orange")  



ggplot(airquality, aes(Temp)) +   geom_bar()  + facet_grid(Month~.)

ggplot(airquality, aes(Temp)) +   geom_bar()  + facet_grid(.~Month)

##### Ejercicio
# Haz un histograma de las temperaturas en NY por mes (con facetas)

ggplot(airquality, aes(Temp)) +   geom_histogram(bins=12)  + facet_grid(Month~.)

ggplot(airquality, aes(Temp)) +   geom_histogram(bins=12)  + facet_grid(.~Month)




##### Ejercicio
# Prueba con los gráficos de violín (que son una alternativa moderna y útil en ocasiones 
# a los gráficos de cajas y que comparten sus ventajas con las de los histogramas).


ggplot(airquality, aes(x = factor(Month), y = Temp)) +   geom_violin(stat = "ydensity")  + facet_grid(Month~.)

ggplot(airquality, aes(x = factor(Month), y = Temp)) +   geom_violin(fill='steelblue')  + facet_grid(.~Month)

ggplot(airquality, aes(x = factor(Month), y = Temp, fill=Month)) +   geom_violin()  + facet_grid(.~Month)

ggplot(airquality, aes(x = factor(Month), y = Temp, fill=Month)) +   geom_violin()  + facet_null()








##### Ejercicio
# Superpón las distribuciones de las temperaturas de NY por mes como se hace [aquí](http://www.datanalytics.com/2015/07/09/son-normales-las-alturas-de-los-individuos/)

tmp_1 <- airquality[,c('Temp','Month')]
tmp_1$Month<-factor(tmp_1$Month)
ggplot (tmp_1, aes(x = Temp, fill= Month)) +   geom_density() 

ggplot (tmp_1, aes(x = Temp, fill= Month)) +   geom_density(alpha= 0.3) 

ggplot (tmp_1, aes(x = Temp, fill= Month)) +   geom_density(alpha= 0.3) + facet_grid(.~Month)

ggplot (tmp_1, aes(x = Temp, fill= Month)) +   geom_density(alpha= 0.3) + facet_grid(Month~.)


##### Ejercicio
# Haz gráficos con tus propios datos.


### Cruces de tablas

# Con la función `merge` pueden hacerse cruces de tablas. Para ilustrar los distintos 
# tipos de cruce, crearemos unos datos pequeños de ejemplo:

clientes <- data.frame(id = 1:3, nombre = c("Carlos", "Sara", "Raquel"))
ventas <- data.frame(fecha = c(1, 1, 1, 2, 2, 3, 3, 3, 4), id = c(1,2,3,2,3, 1, 2, 3, 3), total = 100 * runif(9))

# El código que aparece a continuación crea tablas cruzando las dos anteriores de distintas 
# maneras:
# merge cruza por defecto los nombres de columnas iguales

merge(clientes, ventas)
merge(clientes, ventas, all.x = TRUE)    ## left join

ventas.2 <- ventas[ventas$fecha == 2,]
merge(clientes, ventas.2)
merge(clientes, ventas.2, all.x = T)

##### Ejercicio
# ¿Y si las variables de cruce no se llaman igual? ¿Y si no quieres cruzar por todas 
# ellas?
merge(clientes, ventas , by.x = "id" ,by.y= "id"  )


# Más adelante usaremos los paquetes `dplyr` y `data.table`, que tienen también funciones 
# adicionales para cruzar tablas.



### Web scraping

# Para descargar datos de páginas web usaremos el paquete `rvest`.

library(rvest)

# Vamos a descargar las cotizaciones del IBEX 35 en _tiempo real_:

url.ibex <- "http://www.bolsamadrid.es/esp/aspx/Mercados/Precios.aspx?indice=ESI100000000"

tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")


url <- "http://www.ceroacero.es/edition_matches.php?id_edicao=87618"
tmp2 <- read_html(url)
tmp2 <- html_nodes(tmp, "table")

##### Ejercicio
# Examina los objetos anteriores.

##### Ejercicio
# Averigua cuál es la tabla de interés. Cuando lo averigues, cambia en el código siguiente 
# el interrogante por el valor adecuado y ejecuútalo

#    ibex <- html_table(tmp[[??]])




sapply(tmp, function(x) dim(html_table(x,fill=T )))


ibex <- html_table(tmp[[5]])

ibex 



##### Ejercicio
# Dales nombres _razonables_ a las columnas.

colnames(ibex) <- c("Nombre","Ultimo","DifPorc","Maximo","Minimo","VolumenEfectivo","VolumenEuro","Fecha","Hora")


##### Ejercicio
# Examina la [documentación del paquete _rvest_](http://cran.r-project.org/web/packages/rvest/rvest.pdf) 
# y busca aplicaciones a sus funciones.


### Manipulación básica de texto

# Las tablas bajadas de internet (y datos procedentes de otras fuentes) exigen en ocasiones 
# un preproceso para modificar tipos de datos. Es típico tener que dar un determinado 
# formato a campos numéricos o de fecha.

## Ejercicio
# Los números que aparecen en la tabla descargada en la sección anterior no tienen 
# formato numérico. Para convertirlos en números _de verdad_, transfórmalos adecuadamente:
# * Usa `gsub` para cambiar "." por "" (i.e., nada) en las columnas de interés.
# * Usa `gsub` para cambiar "," por "." en las columnas de interés
# * Usa as.numeric para cambiar texto por números
# * Crea una función que aplique todas las transformaciones anteriores a un vector 
# y aplícasela a las columnas de interés
# * ¿Te atreves a usar as.Date para cambiar texto por fechas?

ibex$VolumenEfectivo <- as.numeric(gsub(".","",ibex$VolumenEfectivo,fixed=T))

ibex$VolumenEuro <- gsub(".","",ibex$VolumenEuro,fixed=T)
ibex$VolumenEuro <- as.numeric(gsub(",",".",ibex$VolumenEuro,fixed=T))

ibex$Ultimo <- as.numeric(gsub(",",".",ibex$Ultimo,fixed=T))   # otra opcion es gsub("\\.","",df[,x])
ibex$DifPorc <- as.numeric(gsub(",",".",ibex$DifPorc,fixed=T))
ibex$Maximo <- as.numeric(gsub(",",".",ibex$Maximo,fixed=T))
ibex$Minimo <- as.numeric(gsub(",",".",ibex$Minimo,fixed=T))
ibex$Fecha <- as.Date(ibex$Fecha,"%d/%m/%Y")

summary(ibex)

head(ibex)


replacepunto <- function  (x) {
  y <- gsub(".","",x,fixed=T)
}

s<- replacepunto(ibex$VolumenEfectivo)
s

foo <- function  (x) {
  x <- gsub(".","",x,fixed=T)
  x <- gsub(",",".",x,fixed=T)
  x <- as.numeric(x)
}


weekdays.Date(ibex$Fecha)
months  (ibex$Fecha)
year(ibex$Fecha)


print(foo(ibex$VolumenEuro))

ibex

# Otra funcioń muy útil para procesar texto es `paste`, que tiene un comportamiento 
# distinto según se use con el argumento `sep` o `collapse`.

paste("A", 1:6, sep = ",")

paste("Hoy es ", date(), " y tengo clase de R", sep = "")

paste("Hoy es ", date(), " y tengo clase de R", sep = "<>")

paste("A", 1:6, collapse = ",") #

paste("A", 1:6, sep = "<>") #


# Para la operación inversa, la de partir cadenas de texto, se usa la función `strsplit`:

strsplit("Hoy es martes", split = " ")

strsplit(c("hoy es martes", "mañana es miércoles"), split = " ")

# Advierte que esta función devuelve una LISTA de cadenas de texto.

# Esas son las funciones fundamentales para la manipulación básica de texto en R. Existen 
# otras que es encuentran también en otros lenguajes de programación (p.e., `sprintf`, 
# `substr`, etc.). Sin embargo, para la manipulación avanzada de texto se recomienda:
# * Conocer bien los fundamentos de las expresiones regulares.
# * Usar el paquete `stringr`.

# También pueden ser de interés las funciones `separate` y `unite` del paquete `tidyr`. 
# Véase [esto](http://www.gis-blog.com/data-management-with-r-tidyr-part-1/).

dir()

namefil = "pruebas_test_20160527_zaragoza.csv"
parametros <- unlist(strsplit(namefil, split = "_"))
length(parametros)

zona <-unlist( strsplit(parametros[4],split=".",fixed = T))[1]
fecha <- as.Date(parametros[3],"%Y%m%d")
fecha
zona


ficheros <- c("pruebas_test_20160527_zaragoza.csv","pruebas_test_20160422_huesca.csv","pruebas_01_test_20160522_huelva.csv")

fun <- function(x){
  parametros <- unlist(strsplit(x, split = "_"))
  numparam <- length(parametros)
  zona <-unlist( strsplit(parametros[numparam],split=".",fixed = T))[1]
  fecha <- as.Date(parametros[numparam-1],"%Y%m%d")
  data.frame (fecha,  zona )
}

x<- ficheros[1]

fun2 <- function(x){
  tmp <- gsub("\\.csv$","",x)
  tmp <- strsplit(tmp,split="_")[[1]]
  data.frame(fichero=x, fecha = as.Date(tmp[length(tmp)-1],format="%Y%m%d"),zona= tmp[length(tmp)])
  }



do.call(rbind,lapply(ficheros, fun))#rbind es reduce, el lapply es el map

# otra opcion  
res <- ldply(ficheros,fun)
res


fun(ficheros[1])
fun(ficheros[2])
fun(ficheros[3])

lf <- vapply(ficheros, fun,x)
