#############################################################################
# Ciencia de datos - R - Parte 12: regresión lineal y logística
# cgb@datanalytics.com, 2016-02-15
#
# El objetivo de esta sesión es aprender los fundamentos de modelos de regresión
#   y clasificación alternativos a los clásicos (regresión lineal y logística):
#   árboles, bosques y k-vecinos
#############################################################################

library(party)
library(randomForest)
library(class)

# datos brutos (los dejo siempre en memoria "tal cual")
aceites <- read.table("/home/dsc/Downloads/data/Olive.txt", header = T, sep = "\t")

# datos limpios

dat.region <- aceites
dat.region$Test.Training <- NULL
dat.region$Area <- NULL

arbol.region <- ctree(Region ~ ., data = dat.region)
plot(arbol.region)

# Ejercicio: repetir el ejercicio tratando de predecir la variable Area

dat.region2<- aceites
dat.region2$Test.Training <- NULL
dat.region2$Region<- NULL
arbol.region2 <- ctree(Area ~ ., data = dat.region2)
plot(arbol.region2)

reales <- dat.region2$Area
predichos <- predict(arbol.region2)


sum(reales != predichos)
mean(reales != predichos)
table(reales, predichos)



# ¿Cuántos errores cometemos?
reales <- dat.region$Region
predichos <- predict(arbol.region)

# varias maneras de ver el error
sum(reales != predichos)
mean(reales != predichos)
table(reales, predichos)


# Ejercicio: ¿cuántos errores comete la predicción del área?

reales <- dat.region$Region
predichos <- predict(arbol.region)

# varias maneras de ver el error
sum(reales != predichos)
mean(reales != predichos)
table(reales, predichos)





#random forest


dat.area <- aceites
dat.area$Test.Training <- NULL
dat.area$Region <- NULL

arbol.area <- randomForest(Area ~ ., data = dat.area)
plot(arbol.area)

table(dat.area$Area,predict(arbol.area))
mean(dat.area$Area != predict(arbol.area))
sum(dat.area$Area != predict(arbol.area))


arbol.area$confusion
arbol.area$forest
arbol.area$predicted
class(arbol.area$predicted)
str(arbol.area$predicted)
arbol.area$votes
arbol.area$classes
arbol.area$forest$xlevels
arbol.area$importance
arbol.area$call
arbol.area$call

varImpPlot(arbol.area)

###############################################################################################
# curva ROC (basado en http://freakonometrics.hypotheses.org/9066)
###############################################################################################

datos <- read.table("http://freakonometrics.free.fr/db.txt", header = TRUE, sep = ";")

tmp <- rep("ACD", length(datos$X3))
tmp[datos$X3 %in% c("B","E")] <- "BE"

datos$X3bis <- as.factor(tmp)
modelo <- glm(Y ~ X1 + X2 + X3bis, family = binomial, data = datos)

S <- predict(modelo, type = "response")

roc.curve=function(s, real, predicho, print = FALSE){
 Ps <- (predicho > s)*1
 
 FP <- sum((Ps == 1)*(real == 0))/ sum(real == 0)
 # FP: de todos los noes (reales), a cuántos les asigno Sí
 
 TP <- sum((Ps == 1)*(real == 1))/ sum(real == 1)
 # TP: de todos los síes (reales), a cuántos les digo que sí
 
  if (print == TRUE){
      print(table(Observed = real, Predicted = Ps))
  }
  vect = c(FP,TP)
  names(vect) = c("FPR","TPR")
  return(vect)
}

threshold = 0.5
roc.curve(threshold, datos$Y, S, print = TRUE)


res <- sapply(seq(0,1,by = .01), roc.curve, datos$Y, S)

plot(res[1,], res[2,], col= "grey", lwd = 2, type = "l",
     xlab = "FPR", ylab = "TPR")


###############################################################################################
# entrenamiento y validación
###############################################################################################

# Típicamente, para crear y validar modelos, dividiremos nuestros datos en dos subconjuntos: 
#   entrenamiento, sobre el que construiremos el modelo
#   validación, en el que mediremos el error cometido

# nos gustarán aquellos modelos en los que el error en validación sea el menor posible

# escogemos el 80% para entrenamiento (p.e.)

filas.entrenamiento <- sample(1:nrow(aceites), 0.8 * nrow(aceites))

aceites.train <- aceites[filas.entrenamiento,]
aceites.test  <- aceites[-filas.entrenamiento,]


# ejercicio: partir "student-mat.csv" en entrenamiento / validación (70/30)


###############################################################################################
# comparación de modelos sobre entrenamiento y validación
###############################################################################################

aceites.train$Test.Training <- aceites.train$Region <- NULL

modelo.ctree <- ctree(Area ~ ., data = aceites.train)

reales <- aceites.test$Area
predichos <- predict(modelo.ctree, aceites.test)

table(reales, predichos)
error.rate <- mean(reales != predichos)
error.rate



# con Random Forest

filas.entrenamiento <- sample(1:nrow(aceites), 0.8 * nrow(aceites))

aceites.train <- aceites[filas.entrenamiento,]
aceites.test  <- aceites[-filas.entrenamiento,]


aceites.train$Test.Training <- aceites.train$Region <- NULL

modelo.ctree <- randomForest(Area ~ ., data = aceites.train)

reales <- aceites.test$Area
predichos <- predict(modelo.ctree, aceites.test)

table(reales, predichos)
error.rate <- mean(reales != predichos)
error.rate


# Ejercicio: comparar los modelos lineales dist ~ speed y dist ~ speed + I(speed^2) usando entrenamiento y 
#   validación. Comparadlos en términos del rmse en el conjunto de validación.


###############################################################################################
# sobreajuste
###############################################################################################

modelo.profundo <- ctree(Area ~ ., data = aceites.train, control = ctree_control(minbucket = 1, 
                                                                                 mincriterion = 0.1, 
                                                                                 minsplit = 2))

# Error en el conjunto de entrenamiento
mean(aceites.train$Area != predict(modelo.ctree))
mean(aceites.train$Area != predict(modelo.profundo))

# Error en el conjunto de validación
mean(aceites.test$Area != predict(modelo.ctree, aceites.test))
mean(aceites.test$Area != predict(modelo.profundo, aceites.test))


# En resumen:
#   un modelo demasiado simple lo hace mal en entrenamiento y en validación
#   un modelo demasiado complejo lo hace muy bien en entrenamiento y mal en validación
#   el modelo "ideal" no es ni demasiado simple ni demasiado complejo


###############################################################################################
# jackknife
###############################################################################################

preds <- sapply(1:nrow(cars), function(i){
  predict(lm(dist ~ speed, data = cars[-i,]), cars[i,])
})

rmse.0 <- sqrt(mean((cars$dist - preds)^2))

# Ejercicio: calcular el rmse para el modelo dist ~ speed + I(speed^2) y comparadlos.



###############################################################################################
# selección de modelos (con k-vecinos)
###############################################################################################

aceites.test$Test.Training <- aceites.test$Region <- NULL

test <- function(k){
  res <- knn(aceites.train[,-1], aceites.test[,-1], aceites.train$Area, k = k)
  mean(res != aceites.test$Area) 
}

k.values <- 1:20
errores <- sapply(k.values, test)
plot(k.values, 100 * errores, type = "l", 
     main = "% error en validación según el\nnúmero de vecinos", xlab = "número de vecinos", ylab = "% de error")


res2 <- knn(aceites.train[,-1], aceites.test[,-1], aceites.train$Area, k = 3)

print(res2)

res2 <- as.data.frame(res2)

length(aceites.test)
dim(aceites.test)

head(reales)

head(preds)


# Ejercicio: en lugar de usar entrenamiento y validación, usar la función knn.cv para 
#   hacer un jackknife (ver primero ?knn.cv)


preds <- sapply(1:nrow(aceites), function(i){
  knn(aceites.train[-i,-1], aceites.test[i,-1], aceites.train$Area[-i], k = 3)
})

aceites.train$Area[-3]
i<-5
aceites.train[-i,-1], aceites.test[i,-1], aceites.train$Area[-i]

knn(aceites.train[-i,-1], aceites.test[i,-1], aceites.train$Area[-i], k = 3)

#para normalizar hay que normalizar el conjunto de entrenamiento y aplicar
#los parámetros a los datos de validación


###############################################################################################
# validación cruzada
###############################################################################################

ids <- rep(1:10, length.out = nrow(cars))
ids
table(ids)

# Ejercicio: ¿qué pasa si length.out no es múltiplo de 10?

ids <- sample(ids)      # las aleatorizo
ids

preds.cv <- lapply(unique(ids), function(i){
  preds <- predict(lm(dist ~ speed, data = cars[ids != i,]), cars[ids == i,])
  data.frame(preds = preds, real = cars[ids == i,]$dist)
})

preds.cv <- do.call(rbind, preds.cv)

rmse.0 <- sqrt(mean((preds.cv$preds - preds.cv$real)^2))

rmse.0

# Ejercicio: calcular el rmse para el otro modelo


###############################################################################################
# Introducción al paquete caret
###############################################################################################

library(caret)

# Dos ejemplos basados en 
# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/

## validación cruzada con lm 

# train the model
train.control <- trainControl(method = "cv", number = 10)
model <- train(dist ~ speed, data=cars, trControl=train.control, method="lm")
# summarize results
print(model)  


# validación cruzada y selección del k óptimo con k-vecinos

train.control <- trainControl(method = "cv", number = 10)
grid <- expand.grid(k = 1:10)
model <- train(dist ~ speed, data = cars, trControl = train.control, method = "knn", tuneGrid = grid)
print(model)

# Ejercicio: ajustar modelos de los planteados hoy usando caret y las distintas opciones que
#   aparecen en http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/


  