salticos <- rnorm(1000)
plot(cumsum(salticos),type="l")

mean(rnorm(1000))

curve(dcauchy,-5,5)


mean(rcauchy(100,-5,5))


#mezcla de distribuciones lognormal y poison

foo <- function()
  {
n <-rpois(1,3000)
sum(exp(rnorm(n,3,4)))
}

res <- replicate(100000,foo())

hist(res,breaks=100)

head(res)



#estimar el total de los patos

res <- sapply(1000:10000, function(x)
  sum(sample(1:x,100)<=100)
  )

#plot(1000:10000)
plot(1000:10000,res)

boxplot(res,1000:10000)

boxplot(1000:10000 ~ res)


head(res)
