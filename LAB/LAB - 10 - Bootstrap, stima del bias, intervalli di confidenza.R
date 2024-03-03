## LABORATORIO 10
#

x <- rnorm(100, 175,8)
plot(ecdf(x))
xx <- seq(140,210,1)
lines(xx, pnorm(xx, mean = 175, sd = 8), col = 2, lwd = 3)

#esempio 8.2####

library(bootstrap)
data("law")
# data("law82")
cor(law$LSAT , law$GPA)  # correlazione del campione osservato
# cor(law82$LSAT , law82$GPA) # correlazione della popolazione

# stimare bias e standard error con metodo bootstrap

n <- nrow(law)
rho.hat <- cor(law$LSAT , law$GPA) # rho stimato sul campione
rho.b <- NULL # vettore per accumulare le repliche boostrap 
B <- 200

for (b in 1:B){
  i <- sample(1:n, size = n, replace = TRUE)
  LSAT_b <- law$LSAT[i]
  GPA_b <- law$GPA[i]
  rho.b[b] <- cor(LSAT_b , GPA_b)
}

hist(rho.b) # stima distribuzione boostrap di rho.hat
bias.b <- mean(rho.b) - rho.hat
se.b <- sd(rho.b)

# IC basic:
alpha <- 0.05
2*rho.hat -  quantile(rho.b, c(1-alpha/2, alpha/2)) 
rho.hat
# IC percentile
quantile(rho.b, c(alpha/2, 1-alpha/2))

#esempio 8.5####

library(bootstrap)
data("patch")
patch

n <- nrow(patch)
theta.hat <- mean(patch$y)/mean(patch$z) # theta stimato sul campione
theta.b <- NULL # vettore per accumulare le repliche boostrap 
B <- 200

for (b in 1:B){
  i <- sample(1:n, size =n, replace = TRUE)
  y_b <- patch$y[i]
  z_b <- patch$z[i]
  theta.b[b] <- mean(y_b) / mean(z_b)
}

hist(theta.b) # stima distribuzione boostrap di rho.hat
bias.b <- mean(theta.b) - theta.hat
se.b <- sd(theta.b)


# IC basic:
alpha <- 0.05
2*theta.hat -  quantile(theta.b, c(1-alpha/2, alpha/2)) 
theta.hat
# IC percentile
quantile(theta.b, c(alpha/2, 1-alpha/2))