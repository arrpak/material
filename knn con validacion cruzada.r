library(party)
library(randomForest)
library(class)

aceites <- read.table("/home/dsc/Downloads/data/Olive.txt", header = T, sep = "\t")


ids <- rep(1:10, length.out = nrow(aceites))
ids
table(ids)


ids <- sample(ids)      # las aleatorizo
ids


aceites$Test.Training <- NULL
aceites$Region <- NULL

k=3

mi.cv <- function(k){
  ids <- sample(ids)  
  
  preds.cv <- lapply(unique(ids), function(i){
  preds <- knn(aceites[ids !=i,-1], aceites[ids==i,-1], aceites$Area[ids !=i], k = k)
  data.frame(preds = preds, real = aceites$Area[ids == i])
})

preds.cv <- do.call(rbind, preds.cv)
sum(preds.cv$real != preds.cv$preds)
}

k.values <- 1-.20
res <- sapply(k.values,mi.cv)
plot(res)

