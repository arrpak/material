## author: "Carlos J. Gil Bellosta"
## date: "2016-06-04"

knitr::opts_chunk$set(echo = TRUE)

### Introducción

# El objetivo de esta sección es la de familiarizarnos con los paquetes siguientes:

library(data.table)
library(dplyr)

# Esencialmente, nos permiten hacer lo mismo que ya sabíamos hacer con la función `ddply` 
# del paquete `plyr`, con tres diferencias:

# * Usan una sintaxis distinta
# * Son mucho más rápidos (sobre todo, `data.table`)
# * Permiten operar con datos de un tamaño mucho mayor

### Agregaciones

# El objetivo de esta sección es aprender a realizar operaciones tales como

res <- ddply(paro, .(Gender, Periodo, Situation), summarize, total = sum(value))

# usando estos paquetes alternativos. Para ello usaremos de nuevo el conjunto ya conocido 
# de datos del paro procedentes del INE:

paro <- read.table("/home/dsc/Downloads/data/paro.csv", header = T, sep = "\t")

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

#### Agregaciones con data.table

# En primer lugar, vamos a crear un objeto de la clase `data.table`, que es un tipo 
# particular de tabla (i.e., `data.frame`). De hecho, un `data.table` es también un 
# `data.frame` con la salvedad de que admite operaciones adicionales.

paro.dt <- data.table(paro)

# La función `tables` muestra una lista de los `data.tables` cargados en el entorno.

tables()

# Los `data.tables` están pensados para almacenar grandes volúmenes de datos. Por eso, 
# no hay que tener miedo en en escribir

paro.dt

# porque la expresión anterior solo muestra unas cuantas filas del fichero.

# Para la agregación de datos se usa una extensión del operador `[]` propia de `data.table`:

res <- paro.dt[, list(total = sum(value)), by = c("Gender", "Periodo", "Situation")]

# Hay que tener en cuenta de que los corchetes no funcionan exactamente igual con `data.tables` 
# que con las tablas habituales.

# La manera alternativa de realizar la agregación es mediante la creación de un índice 
# (que es una de las aportaciones fundamentales del paquete `data.table`) sobre la 
# tabla:

setkeyv(paro.dt, c("Gender", "Periodo", "Situation"))

# En realidad, se trata de un índice físico: los datos se ordenan físicamente dentro 
# de la tabla de acuerdo con las variables seleccionadas. Una vez creado el índice, 
# la función `tables` da cuenta de ello:

tables()

# La agregación por índice se hace así:

res <- paro.dt[, list(total = sum(value)), by = key(paro.dt)]

# y, obviamente, se pueden solicitar más de un campo calculado mediante

res <- paro.dt[, list(total = sum(value), max.value = max(value)), by = key(paro.dt)]

#### Agregaciones con dplyr

# La sintaxis de `dplyr`, un paquete creado por el autor de `plyr` es una especie de 
# híbrido entre la de este paquete y la de SQL:

tmp <- group_by(paro, Gender, Periodo, Situation)
res <- summarise(tmp, total = sum(value), max.value = max(value))
class(tmp)
# En realiad, nadie usa la sintaxis anterior sino una basada en _tuberías_:

res <- tmp %>% 
  group_by(Gender, Periodo, Situation) %>%
  summarise(total = sum(value), max.value = max(value))

# Las _tuberías_ (o _pipes_) permiten construir una sintaxis similar a la de los métodos 
# de Java, Python, Scala y otros lenguajes. Sin embargo, el operador `%>%` es muy simple: 
# coloca lo que tiene a la izquierda como argumento de la función que está a la derecha:

foo <- function(x) x + 2
5 %>% foo

# En resumen, `x %>% f(a,b,c)` es lo mismo que `f(x,a,b,c)`.

##### Ejercicio

# Reescribe la expresión
#   tmp <- group_by(paro, Gender, Periodo, Situation)
#   res <- summarise(tmp, total = sum(value), max.value = max(value))
# en una sola línea sin usar tuberías.

res <- summarise(group_by(paro, Gender, Periodo, Situation), total = sum(value), max.value = max(value))

##### Ejercicio
# En `dplyr` existen las funciones `filter` y `select`. Úsalas para obtener el agregado 
# anterior solo para las mujeres (y excluyendo la variable "Gender") utilizando tuberías.

resmujeres <- select(res,)


##### Ejercicio

# El objetivo de este ejercicio es que compares `plyr` con los otros dos (y estos entre 
# sí) y que practiques la sintaxis con los siguientes conjuntos de datos:
# * Un fichero de la EPA. Para ello, acude al [portal de microdatos INE](http://www.ine.es/prodyser/microdatos.htm) 
# y localiza los de la EPA (encuesta de población activa). Baja los de un trimestre 
# y utiliza la función `epa2005` del paquete `MicroDatosEs` para leerlos. Recuerda 
# que para hacer agregaciones tienes que sumar la variable `factorel`.
# * Parte (o todo el censo español del 2011). En la misma página del INE enlazada más 
# arriba, accede a los microdatos del censo. Puedes intentar utilizar los datos nacionales 
# o uno de los subconjuntos más manejables. Utiliza la función `censo2010` del paquete 
# `MicroDatosEs` aunque, antes de eso, lee muy detenidamente la ayuda. Recuerda de 
# nuevo que para contar hay que sumar la variable `factor`.
# * El fichero de [retrasos de vuelos en EE.UU.](http://stat-computing.org/dataexpo/2009/the-data.html) 
# para un determinado año. Prueba a leerlos con la función `fread` de `data.table`, 
# que es mucho más rápida (aunque menos flexible) que `read.table`.


### Transformaciones por bloques

# Para obtener el periodo con mayor tasa de paro (tanto masculina como femenina) en 
# cada provincia se puede utilizar el paquete `plyr` así:

library(reshape2)

tasa.paro <- dcast(paro, Gender + Provinces + Periodo ~ Situation)
tasa.paro <- transform(tasa.paro, tasa.paro = unemployed / active)
tasa.paro <- tasa.paro[, c("Gender", "Provinces", "Periodo", "tasa.paro")]

tmp <- ddply(tasa.paro, .(Gender, Provinces), transform, rank = rank(-tasa.paro, ties = "random"))
res <- tmp[tmp$rank == 1,]

#### Transformaciones por bloques usando data.table

# Para realizar operaciones por bloques se usa el operador `:=`.

tasa.paro.dt <- data.table(tasa.paro)

tmp <- tasa.paro.dt[, rank := rank(-tasa.paro, ties = "random"), by = c("Gender", "Provinces")]
res <- tmp[tmp$rank == 1, ]
res$rank <- NULL
res

# En el siguiente ejemplo usaremos datos del número de desempleados por periodo, provincia 
# y sexo,

parados <- paro[paro$Situation == "unemployed", ]

# Para calcular el porcentaje de ellos que son hombres/mujeres:

parados.dt <- data.table(parados)
res <- parados.dt[, pct := 100 * value / sum(value), by = c("Periodo", "Provinces")]
res

#### Transformaciones por bloques con dplyr

res <- tasa.paro %>% group_by(Gender, Provinces) %>% 
  mutate(rank = rank(-tasa.paro, ties = "random")) %>% filter(rank == 1) %>%
  select(-rank)

# Por su parte, el porcentaje de parados se calcula así:

res <- parados %>% group_by(Periodo, Provinces) %>% mutate(pct = 100 * value / sum(value))
res

##### Ejercicio
# En cada periodo, calcula el porcentaje de parados que aporta cada provincia (de manera 
# que la suma de los porcentajes de todas las provincias sumen el 100% en cada periodo. 
# Hazlo para el total por sexos (hombres + mujeres). Usa los tres paquetes: `plyr`, 
# `data.table` y `dplyr`.



### Cruces de tablas

# Vamos a crear unos datos simples aleatoriamente: una lista de contratos con sus importes 
# y clientes asociados. Cada cliente puede tener uno o más contratos y cada contrato 
# tiene un único importe.

n <- 6e6
n.clientes <- 1e6

contracts <- data.frame(
  customer  = sample(n.clientes, n, replace = T),
  contract  = sample(n, n),
  amount    = 1000 * exp(runif(n))
)

# El objetivo será asignar a cada contrato el porcentaje de facturación que corresponde 
# a su cliente. Con `data.table` puede hacerse directamente así:

contracts.dt <- data.table(contracts)
setkey(contracts.dt, "customer")
res <- contracts.dt[, pct.customer := amount / sum(amount), by = key(contracts.dt)]

##### Ejercicio
# Resuelve el mismo problema az lo mismo con `dplyr`.

##### Ejercicio
# Mide los tiempos de ejecución de ambas soluciones con `system.time`.

# Hay una manera menos eficaz de hacer lo anterior. Por ejemplo, sin los paquetes usados 
# más arriba se haría así:

customer.total <- xtabs(amount ~ customer, data = contracts)
customer.total <- as.data.frame.table(customer.total)
colnames(customer.total) <- c("customer", "total.amount")
customer.total$customer <- as.numeric(as.character(customer.total$customer))

res <- merge(contracts, customer.total)
res$pct.customer <- 100 * res$amount / res$total.amount

##### Ejercicio:
# Identifica el cuello de botella en el código anterior.

# Aunque sabemos ya hacerlo mejor, el ejercicio es una excusa para practicar los cruces 
# de tablas replicando con estos nuevos paquetes el código _tradicional_. Con `data.table`, 
# hay que crear primero índices por los campos de cruce y después utilizar `merge`.

contracts.dt      <- data.table(contracts,      key = "customer")
customer.total.dt <- data.table(customer.total, key = "customer")

res <- merge(contracts.dt, customer.total.dt)       # ¡cruza por key! (para eso tienen que tener la misma)

rm(contracts.dt, customer.total.dt)

# Este `merge` es mucho más rápido que el tradicional porque realiza una búsqueda por 
# índice.

# Con `dplyr` se puede utilizar directamente la función `join`:

# res <- join(contracts, customer.total)

##### Ejercicio
# Investiga por qué campos se realiza `join` el cruce.

##### Ejercicio
# En la `?join` aparecen otras _funciones hermanas_. ¿Te suenan de algo? Revisa [esta 
# página](https://stat545-ubc.github.io/bit001_dplyr-cheatsheet.html). 


### Otros recursos sobre ambos paquetes

# `data.table` incluye la función `fread`, que es mucho más rápida que `read.table` 
# para datos grandes. `data.table` incluye además otras funciones para acceso rápido 
# a registros individuales en tablas. Puedes consultarlas [aquí](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.pdf).

# `dplyr` contiene otras funciones que puedes consultar [aquí](https://rpubs.com/justmarkham/dplyr-tutorial).