#############################################################################
# Ciencia de datos - R - Parte 01: un paseo por R (+ tablas)
# cgb@datanalytics.com, 2016-05-13
#
# El objetivo de esta sesión es recorrer algunas de las principales aplicaciones
# de R: manipulación de datos, gráficos y estadística. Además, aprenderemos 
# a realizar operaciones habituales y conocidas que hacemos regularmente con Excel 
# sobre tablas usando R.
#############################################################################

#----------------------------------------------------------------------------
# Tablas (dataframes)
#----------------------------------------------------------------------------

## Inspección de un dataframe

# Primera tarea: inspeccionar los contenidos de una tabla 

# El conjunto de datos iris viene de serie en R. Tiene 150 filas que recogen datos de 
#   150 iris (una flor: http://es.wikipedia.org/wiki/Iris_%28planta%29) indicando algunas
#   características de ellas. Este conjunto de datos permite crear un modelo estadístico
#   para distinguir subespecies de iris en función de características métricas de la flor.

iris

# o bien

print(iris)

# otras funciones útiles para inspeccionar data.frames

plot(iris)       # lo representa gráficamente
summary(iris)    # resumen estadístico de las columnas
str(iris)        # "representación textual" del objeto

# ver parte de una tabla: 

head(iris)    # primeras seis filas
tail(iris)    # últimas seis filas

# ayuda en R
?summary

# Ejercicio: Consulta la ayuda de la función head y averigua
# cómo mostrar las diez primeras filas de iris en lugar de las
# seis que aparecen por defecto.

head(iris,10)
d=5L  #forma de definir un entero

# tamaño de una tabla:
dim(iris)  
nrow(iris) 
ncol(iris)

# nombre de las columnas de una tabla
colnames(iris)  


## Selección de filas y columnas

# con corchetes
iris[1:10,]   # selecciona fila de la 1 a las 10 
iris[,3:4]    # selecciona columna de la 3 a la 4 
iris[1:10,3:4]

#los indices empiezan por 1

# Importante: para un dataframe, los corchetes siempre tienen dos partes separadas por una coma:
#   - lo que va antes de la coma se refiere a las filas
#   - lo que va tras la coma, a las columnas

# con corchetes usando el nombre de las columnas
iris[, "Species"] 

# otros operadores: dobles corchetes y dólar, las 3 formas son equivalentes
# ojo que lleda doble corchete

iris[[5]]   #selecciona por numero de columna
iris[["Species"]]  #selecciona por nombre se usa por si hay espacios en el nombre de col.
iris$Species    #

# Nota: una de las mayores diferencias entre [] y [[]]/$ es que [] permite seleccionar
#   varios elementos, mientras que [[]] y $ solo dejan seleccionar uno.

#REPASAR ESTO
c(iris$Species,iris$Sepal.Length)

#un dataframe es una lista de vectores



# con la función subset
subset(iris, Species == "setosa") 
subset(iris, select = Species) #selecciona la columna especies 
subset(iris, select = -Species)   # todo menos especies

# con condiciones lógicas
iris[iris$Species == "setosa",]

# Nota: los ejemplos anteriores dan cuenta de la versatilidad del corchete. Dentro de él pueden 
#   indicarse:
#   - coordenadas (o rangos de coordenadas) de filas y columnas
#   - nombres de columnas
#   - condiciones lógicas para seleccionar filas que cumplan un criterio


# Es posible crear otras tablas a partir de una dada:

iris[iris$Sepal.Length > 7.7,]
iris[iris$Sepal.Length ==  min(iris$Sepal.Length),]
iris[iris$Sepal.Length <  1.1*min(iris$Sepal.Length),]

#para las fechas es mejor usar <- (ctrl+guion) en lugar de =

mi.iris <- iris  # mi.iris es una copia de iris
head(mi.iris)

# ¿qué hay en memoria?
View(iris)

ls()
rm(d)
rm(mi.iris)
ls()

rm(list=ls())

?rm

## Ordenación

# por una columna

mi.iris <- iris[order(iris$Petal.Length),]

# Nota: R no trae ninguna función "de serie" para ordenar por una columna (o varias columnas)
#   Más adelante veremos funciones en paquetes adicionales para hacerlo.
#   En R base "ordenar" es "seleccionar FILAS ordenadamente"

# Ejercicio: verificar que mi.iris <- iris[order(-iris$Petal.Length),] ordena decrecientemente

# Ejercicio: R permite ordenar por dos columnas porque order lo permite (ver ?order). Por ejemplo,
#   iris[order(iris$Petal.Length, iris$Sepal.Length),]
#   deshace los empates en Petal.Length de acuerdo con Sepal.Length.
#   Crea una versión de iris ordenando por especie y dentro de cada especie, por Petal.Length


## Creación y eliminación de columnas

mi.iris$Petal.Area <- mi.iris$Petal.Length * mi.iris$Petal.Width
mi.iris$Petal.Area <- NULL

# Nota: agregar una columna que no existe la crea
#   agregar una columna que existe ¡la reemplaza!
#   asignar NULL a una columna existente la elimina

# la función transform también es útil para crear nuevas columnas
mi.iris <- transform(mi.iris, Petal.Area = Petal.Length * Petal.Width, Sepal.Area = Sepal.Length * Sepal.Width)
#ojo q usa el . en lugar de $



# Ejercicio: estudia el conjunto de datos "airquality" (información meteorológica de 
#   cierto año en Nueva York) aplicando las funciones anteriores. En particular, responde a:
#   - ¿Cuál es la temperatura media de esos días?
#   - ¿Cuál es la tempreatura media en mayo?
#   - ¿Cuál fue el día más ventoso?

airquality
median(airquality$Temp)
mean(airquality$Temp)
mean(airquality[airquality$Month=='5',]$Temp)

airquality[airquality$Wind==max(airquality$Wind),]



# Ejercicio: crea una tabla adicional seleccionando todas las columnas menos mes y día; luego
#   haz un plot de ella y trata de encontrar relaciones (cualitativas) entre la temperatura y el viento
#   o el ozono,...

subset(airquality ,select = -c(Month,Day))

plot(airquality)

#----------------------------------------------------------------------------
# Lectura de datos externos (.csv y similares)
#----------------------------------------------------------------------------

getwd()
setwd("..")       # "sube" al directorio anterior
dir()             # contenidos del directorio "de trabajo"

setwd("/home/dsc/Downloads/")


# La mejor manera de especificar el directorio de trabajo en RStudio es usando
#   los menús (Files > More > Set as working directory)

# Ejercicio: lee el fichero paro.csv usando la función read.table. Comprueba que
#   está correctamente leído usando head, tail, nrow, summary, etc.
#   Pista: ?read.table
#   Pista: los parámetros más importantes son header y sep; a veces, dec y quote para las comillas.

?read.table

paro<-read.table("data/paro.csv",header=TRUE,sep='\t',dec='.')
#header=FALSE por defecto
head(paro)

summary(paro)

dim(paro)


#esto lee lineas sin separador

rl<-readLines("data/carburantes_20050222.csv")
rl[208]


carburantes <-read.table("data/carburantes_20050222.csv",header=TRUE,sep='\t',dec=',',quote='')

plot(carburantes$X.Longitud..WGS84..,carburantes$X.Latitud..WGS84..)

gasmadrid <-carburantes[carburantes$Provincia=='MADRID',]
plot(gasmadrid$X.Longitud..WGS84..,gasmadrid$X.Latitud..WGS84..)



mean(gasmadrid$X.Precio.Gasoleo.A. ,na.rm = TRUE)

?mean





#----------------------------------------------------------------------------
# Gráficos básicos
#----------------------------------------------------------------------------

# Gráficos de dispersión

plot(cars$speed, cars$dist)

plot(cars$speed, cars$dist , type='s')
plot(cars$speed, cars$dist , type='p')
plot(cars$speed, cars$dist , type='l')
plot(cars$speed, cars$dist , type='b')
plot(cars$speed, cars$dist , type='c')

# Ejercicio: representa gráficamente la anchura del sépalo contra su longitud (en iris)
# Ejercicio: usa main, xlab e ylab para añadir etiquetas (ver ?plot)

plot(iris$Sepal.Width,iris$Sepal.Length)

plot(iris$Sepal.Width,iris$Sepal.Length,main="main",xlab="xlab",ylab="ylab")


# más:

plot(cars$dist, main = "Distancias de...", ylab = "distancia en millas")
lines(cars$dist)
grid()
abline(mean(iris$Sepal.Width),1)


# Casi todos los aspectos de un gráfico son parametrizables

# Ejercicio: Consulta la ayuda de abline y úsala para añadir
#   líneas a alguno de los gráficos anteriores

abline(mean(iris$Sepal.Width),1,reg=)

# Ejercicio: usa ??lty para encontrar una página de ayuda en R que muestra
#   gran cantidad de parámetros gráficos. Investiga y usa col, lty y lwd.


## Gráficos de barras

barplot(VADeaths[,2], xlab = "tramos de edad", ylab = "tasa de mortalidad", 
        main = "Tasa de mortalidad en Virginia\nmujer/rural")

# Ejercicio: mejora el gráfico anterior con el parámetro col (de color).

barplot(VADeaths[,2], xlab = "tramos de edad", ylab = "tasa de mortalidad", 
        main = "Tasa de mortalidad en Virginia\nmujer/rural",col="yellow1", border="red")

# Una alternativa:

dotchart(t(VADeaths),  main = "Death Rates in Virginia - 1940")


## Histogramas

hist(iris$Sepal.Width)

# Ejercicio: estudia la distribución de las temperaturas en Nueva York (durante los meses en cuestión)
# Ejercicio: usa "col" para mejorar el aspecto del gráfico.
# Ejercicio: usa abline para dibujar una línea vertical roja en la media de la distribución.

## Diagramas de caja (boxplots)

boxplot(iris$Sepal.Width ~ iris$Species, col = "blue",
        main = "Especies de iris\nsegún la anchura del sépalo", border="yellow")

# Nota: la notación y ~ x es muy común en R y significa que vas a hacer algo con y en función de x; en 
#   este caso, "algo" es un diagrama de caja.

# Ejercicio: muestra la distribución de las temperaturas en Nueva York en función del mes.


#----------------------------------------------------------------------------
# Ejercicios
#----------------------------------------------------------------------------

# Carga el fichero dat/Olive.txt y ferifica que está correctamente cargado
# Selecciona los registros de una región determinada
# Haz un boxplot de "eicosenoic" en función de la región
# Encuentra un "punto de corte"
# Selecciona solo los registros por debajo de ese punto de corte

olive<-read.csv("data/Olive.txt",header=TRUE,sep='\t',dec='.')
#header=FALSE por defecto
head(olive)
olive[olive$Region=='Sardinia',]

bxp<-boxplot(olive$eicosenoic ~ olive$Region, col = "grey",
        main = "Titulo", border="red")
bxp

summary(bxp)

summary(olive)

w=glm(olive$eicosenoic ~ olive$Region,data = datos, weights = Freq, family = binomial())
summary(w)

olive[olive$eicosenoic<23,]


#----------------------------------------------------------------------------
# Estadística
#----------------------------------------------------------------------------

# Test de Student

summary(sleep)
boxplot(sleep$extra ~ sleep$group, col = "gray", 
        main = "Diferencias por grupo")
?t.test
t.test(sleep$extra ~ sleep$group)

# Ejercicio: usar una alternativa no paramétrica: wilcox.test



# Ejercicio: Antes de aplicar el test de Student uno quiere verificar
#   si se cumple la hipótesis de igualdad de las varianzas. Identifica
#   el test necesario (usa, p.e., un buscador de internet) y aplícaselo
#   a sleep


# Regresión lineal
plot(cars$speed, cars$dist)

lm.dist.speed <- lm(cars$dist ~ cars$speed)
lm.dist.speed
abline(lm.dist.speed, col = "red")

summary(lm.dist.speed)

op <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lm.dist.speed)
par(op)


# Ejercicio: Representar gráficamente cars$speed y cars$dist
#   mediante un gráfico de dispersión y añadir luego la recta de regresión
#   usando la función abline. (Pista: hay ejemplos de algo parecido en 
#   ?abline).

# Ejercicio: Hacer una regresión del nivel de ozono sobre la temperatura en Nueva York. 

## Regresión logística

datos <- as.data.frame(UCBAdmissions)
datos$Admit <- datos$Admit == "Admitted"

modelo.sin.dept <- glm(Admit ~ Gender, data = datos, weights = Freq, family = binomial())
summary(modelo.sin.dept)

modelo.con.dept <- glm(Admit ~ Gender + Dept, data = datos, weights = Freq, family = binomial())
summary(modelo.con.dept)
