p <- 0.53
df <- rbinom(10000,1,p)

media <- mean(df)
varianza <- sd(df)**2





estimate.mean <- function(n,p=0.5) mean(rbinom(n,1,p))
res <- replicate(1000,estimate.mean(100000,p=0.6))

estimate.var <- function(n,p=0.5) var(rbinom(n,1,p))
res <- replicate(1000,estimate.mean(100000,p=0.6))


pes <- seq(0,1,by=0.01)
res <- sapply(pes,function(p) estimate.var (1000000,p))
plot(pes,res,typ="l")


hist(res)


library("skellam")



dsk <- rskellam(100000,lambda1 = 2.1,lambda2 = 2.1)
hist(dsk,freq=T,nclass=30)


bineg <- replicate(1000,sum(rnbinom(100000, 1, 0.999)))

hist(bineg,nclass = 40)

1-ppois(75,60)

qpois(0.99,60)

