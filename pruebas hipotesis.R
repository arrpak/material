res <- rbinom(1000, 100, 0.5)
hist(res, breaks = 30)
abline(v = 60, col = "red")

mean(res > 59.5)

#test binomial
binom.test(6, 10, 0.5, "greater")





es <- rbinom(1000, 100, 0.5) / 100   # proporciones
hist(res, breaks = 30, freq = FALSE, main = "")
abline(v = 0.6, col = "red")
curve(dnorm(x, 0.5, 0.5 / 10), from = 0.3, to = 0.7, col = "blue", add = T)











