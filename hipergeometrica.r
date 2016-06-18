#ejercicio peces del lago

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


res <- replicate(10000,sum(sample(1:n,100) <=100))

                 
hist(res)                 
