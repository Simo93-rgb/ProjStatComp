## LABORATORIO 8
#

#esempio 7.1: theta = E[|X_1 - X_2|]####
m <- 1000
x <- matrix(  rnorm(m*2) ,  ncol = 2)
y <- abs( x[,1] - x[,2] )  # valore assoluto
theta.hat <- mean(y)
se.theta.hat <- sd(y)/sqrt(m)

theta.teo <- 2/sqrt(pi)
se.theta.teo <- sqrt( (2 - 4/pi) / m )

c(theta.hat, theta.teo, se.theta.hat, se.theta.teo, 
  se.theta.hat^2, se.theta.teo^2)

#stima MSE del precedente stimatore####

theta.hat <- NULL
for (i in 1:1000){
  m <- 1000
  x <- matrix(  rnorm(m*2) ,  ncol = 2)
  y <- abs( x[,1] - x[,2] )  # valore assoluto
  theta.hat[i] <- mean(y)
}
theta.teo <- 2/sqrt(pi)
mse.theta <- mean( (theta.hat - theta.teo)^2 )
mse.teo <- (2-4/pi)/m + 0^2
c(mse.theta, mse.teo )
c(mean(theta.hat), theta.teo)

#esempio 7.4 e 7.5: stima della copertura di IC per varianza####

m <- 1000   # taglia campione MC
n <- 20     # taglia campione di X per il calcolo di IC
alpha <- .05 # (1-alpha) = livello di significatività o copertura

y <- NULL
for (i in 1:m){
  x <- rnorm(n, mean = 0, sd = 2) # campione delle X
  y[i] <- (((n-1)*var(x) / qchisq(alpha , df = n-1 )) >= 4) *1 
}
theta.hat <- mean(y)
print(c(theta.hat, 1-alpha))   # stima della copertura e valore teorico

# equivalentemente si puo' generare y cosi':
y <- replicate(m, expr = {
  x <- rnorm(n, mean = 0, sd = 2)
  (((n-1)*var(x) / qchisq(alpha , df = n-1 )) > 4) *1 
})

# esempio 7.6
# valuto livello IC quando x = chi-quadro con 2 gradi di liberta'
n <- 20
y <- replicate(m, expr = {
  x <- rchisq(n, df = 2)
  (((n-1)*var(x) / qchisq(alpha , df = n-1 )) > 4) * 1 
})
mean(y)




