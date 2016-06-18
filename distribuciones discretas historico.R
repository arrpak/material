
mean(sample(1:6, 1000000, replace = T) %in% c(2,6))

df <- rbinom(100000, 100, 0.1)
head(df)
summary(df)
plot(df)


n <- 1000
m <- 100
a <- sample(1:n,m)
b <- sample(1:n,m)
res <- replicate(10000,
{
a <- sample(1:n,m)
b <- sample(1:n,m)
length(intersect(a,b))
})


?sample

media <- mean(df)

desv <- std(df)

var <- var(df)

p <- 0.53
df <- rbinom(10000,1,p)
p <- 0.53
df <- rbinom(10000,1,p)


estimate.mean <- function(n,p=0.55) mean(rbinorm(n,1,p))
res <- replicate(1000,estimate.mean(100000,p=0.6))
estimate.mean <- function(n,p=0.55) mean(rbinom(n,1,p))
res <- replicate(1000,estimate.mean(100000,p=0.6))
estimate.mean <- function(n,p=0.55) var(rbinom(n,1,p))
res <- replicate(1000,estimate.mean(100000,p=0.6))



p <- 0.53
df <- rbinom(10000,1,p)
media <- mean(df)
varianza <- sd(df)**2
estimate.mean <- function(n,p=0.55) mean(rbinom(n,1,p))
res <- replicate(1000,estimate.mean(100000,p=0.6))
estimate.mean <- function(n,p=0.55) var(rbinom(n,1,p))
res <- replicate(1000,estimate.mean(100000,p=0.6))



res <- seq(0,1,by=0.01)
estimate.var <- function(n,p=0.55) var(rbinom(n,1,p))
res <- replicate(1000,estimate.mean(100000,p=0.6))
res <- sapply(pes,function(p) estimate.var (1000000,p))
pess <- seq(0,1,by=0.01)
res <- sapply(pes,function(p) estimate.var (1000000,p))
pes <- seq(0,1,by=0.01)
res <- sapply(pes,function(p) estimate.var (1000000,p))
plot(pes,res,typ="l")
plot(pes,res,typ="l")
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
res
hist(res)
install.packages("skellam")
library("skellam")
dsk <- rskellam(100,lambda1 = 2.1,lambda2 = 1.3)
plot(dsk)
hist(dsk)
dsk <- rskellam(100,lambda1 = 2.1,lambda2 = 2.1)
hist(dsk)
dsk <- rskellam(10000,lambda1 = 2.1,lambda2 = 2.1)
hist(dsk)
hist(dsk,freq=T,nclass=100)
hist(dsk,freq=T,nclass=10)
hist(dsk,freq=T,nclass=30)
dsk <- rskellam(10000,lambda1 = 2.1,lambda2 = 2.1)
hist(dsk,freq=T,nclass=30)
dsk <- rskellam(100000,lambda1 = 2.1,lambda2 = 2.1)
hist(dsk,freq=T,nclass=30)
rnbinom(100, 1, 0.2)
rnbinom(10, 1, 0.2)
rnbinom(10, 1, 0.01)
rnbinom(10, 1, 0.999)
rnbinom(10, 1, 0.999)
rnbinom(1000000, 1, 0.999)
sum(rnbinom(1000000, 1, 0.999))
sum(rnbinom(1000000, 1, 0.999))
bineg <- replicate(100,sum(rnbinom(1000000, 1, 0.999)))
hist(bineg)
hist(bineg,nclass = 100)
hist(bineg,nclass = 10)
hist(bineg,nclass = 20)
hist(bineg,nclass = 40)
bineg <- replicate(1000,sum(rnbinom(100000, 1, 0.999)))
hist(bineg,nclass = 40)
dpois(75,60)
pdpois(75,60)
pppois(75,60)
ppois(75,60)
1-ppois(75,60)
table.freq(tmp)
library(agricolae)
install.packages("agricolae")
qpois(0.99,60)
library(agricolae)
library(agricolae)
set.seed(123)
x <- rnorm(100, mean = 2, sd=1)
tmp <- hist(x, plot=FALSE)
table.freq(tmp)
t<-table.freq(tmp)
View(t)
View(t)
t<-table.freq(x)
salticos <- rnorm(1000)
salticos <- rnorm(1000)
plot(sumsum(salticos),type='l')
salticos <- rnorm(1000)
plot(sumsum(salticos),type='l')
plot(sumsum(salticos),type="l")
plot(cumsum(salticos),type="l")
mean(salticos)
mean(rnorm(1000))
mean(rnorm(1000))
mean(rnorm(1000))
mean(rnorm(1000))
