#teorema de bayes
# a_h será a condicionado por b
# accidentes de mujeres


a_h <- 0.1
a_m <- 0.05
m <- 0.45
a <- 0.05*0.45+0.1*0.45

#m_a <- (a_m * m )/ a 

m_a <- 0.05*0.45/a


#raza y delincuencia

#a ser de raza negra
a <- 0.1
#d ser delincuente

d <- 0.01


a_d <- 0.6

# d_a = a_d * d /a

d_a <- 0.6*0.01/0.1


# d_b = b_d * d / b

d_b <- 0.4*0.01 / 0.9

# d_a = 

#  MAL SACAR LA PROB DE D CON PROB TOTAL
# d = d_b * d + d_a * a

a_estim <- 0.4*0.01 / 0.9 * 0.01 + 0.6*0.01/0.1 * 0.01




library(rstan)

stanmodelcode <- '
data {
int<lower=1> N;
int n;
}

parameters {
real<lower=0, upper = 1> p;
}

model {
// priori no informativa la beta 1,1 es una uniforme
p ~ beta(1,1);  

// verosimilitud
n ~ binomial(N, p);  
}
'

fit <- stan(model_code = stanmodelcode, 
            data = list(N = 100, n = 60),
            iter = 12000, warmup = 2000, 
            chains = 4, thin = 10)

# thin coger una de cada 10 simulaciones
# hace 4 veces 12000 simulaciones


res <- as.data.frame(fit)
hist(res$p, breaks = 30, col = "gray", main = "", xlab = "")
abline(v = 0.5, col = "red")



summary(lm(dist~speed,data=cars))


