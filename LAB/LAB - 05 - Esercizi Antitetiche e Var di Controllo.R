## LABORATORIO 5
#

#Esercizio 1####
m <- 1000  # taglia campione
theta.hat <- theta.hat.ant <- NULL
for (i in 1:m){
  u <- runif(m)
  # MC diretto
  theta.hat[i] <- mean(exp(-u)/(1+u^2))
  # MC metodo antitetica
  u[(m/2+1):m] <- 1-u[1:(m/2)]
  theta.hat.ant[i] <- mean(exp(-u)/(1+u^2))
}

c( sd(theta.hat), sd(theta.hat.ant) )
(var(theta.hat)-var(theta.hat.ant))/var(theta.hat) 


#Esercizio 2####

m <- 1000  # taglia campione
theta.hat <- theta.hat.ant <- NULL
for (i in 1:m){
  u <- runif(m)
  # MC diretto
  theta.hat[i] <- pi/3*mean(sin(u*pi/3))
  # MC metodo antitetica
  u[(m/2+1):m] <- 1-u[1:(m/2)]
  theta.hat.ant[i] <- pi/3*mean(sin(u*pi/3))
}

c( sd(theta.hat), sd(theta.hat.ant) )
(var(theta.hat)-var(theta.hat.ant))/var(theta.hat) 