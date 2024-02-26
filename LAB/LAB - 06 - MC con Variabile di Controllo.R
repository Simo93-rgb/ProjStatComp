## LABORATORIO 6
#

#primo esercizio su MC con variabile di controllo####

rm(list = ls())
n <- 100   # num. di repliche dei MC
m <- 1000  # taglia campionaria per il singolo MC
c.star <- -(3 - exp(1))*6 # costante ottimale per MC
theta.sem <- theta.c <- NULL
for (i in 1:n){
  u <- runif(m)
  theta.sem[i] <- mean(exp(u))
  theta.c[i] <- mean( exp(u)+ c.star * (u-1/2) )
}
c(var(theta.sem) , var(theta.c))
(var(theta.sem) - var(theta.c))/var(theta.sem)


#esempio stima c ottimale per met. var. di controllo####

rm(list = ls())
theta.d <- theta.c <- NULL

for (i in 1:200){
  u <- runif(1000) # campione di uniformi
  g.u <- exp(-u)/ (1+u^2)
  f.u <- exp(-.5)/ (1+u^2)
  theta.d[i] <- mean(g.u)  # MC semplice/diretto
  
  c.star <- - cov(f.u, g.u) /var(f.u)
  theta.c[i] <- mean( g.u + c.star * (f.u - exp(-.5)*pi/4)  ) 
}

c(var(theta.d), var(theta.c) )

(var(theta.d) - var(theta.c))/var(theta.d) #

c(sd(theta.d), sd(theta.c) )



#esempio stima c ottimale tramite regressione####
rm(list = ls())
u <- runif(1000) # campione di uniformi
g.u <- exp(-u)/ (1+u^2)
f.u <- exp(-.5)/ (1+u^2)
c.star.0 <- - cov(f.u, g.u) / var(f.u)
L <- lm(g.u ~ f.u)
c.star <- -L$coeff[2]

# theta.c <- mean( g.u + c.star * (f.u - exp(-.5)*pi/4)  ) 
theta.c <- L$coeff[1] + L$coeff[2] * exp(-.5)*pi/4

se.theta.c <- summary(L)$sigma
se.theta.c





