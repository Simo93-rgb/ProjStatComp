## LABORATORIO 07
#
#esercizio 6.10 importance sampling####

I <- function(x){ (x>0)*(x<1) }
g <- function(x){ exp(-x) / (1+x^2) }

f1 <- function(x){ exp(-x)} 
f2 <- function(x){ pi^(-1) * (1+x^2)^(-1)}
f3 <- function(x){ exp(-x) / (1-exp(-1))}
f4 <- function(x){ 4 * f2(x)}

m <- 1000

#CASO f1####
x1 <- rexp(m, rate = 1)  # X ~ exp(1)

theta.hat.1 <- mean( I(x1) * g(x1) / f1(x1) )
# theta.hat.1 <- mean( I(x1) * g(x1) / dexp(x1, rate =1) )

varianza.1 <-var( I(x1) * g(x1) / f1(x1) ) / m


#CASO f2####
x2 <- rcauchy(m)  # X ~ exp(1)
theta.hat.2 <- mean( I(x2) * g(x2) / f2(x2) ) # Identificatrice * pi * exp(-x)

varianza.2 <-var( I(x2) * g(x2) / f1(x2) ) / m

#CASO f3####
u3 <- runif(m)
x3 <- - log(1 - (1-exp(-1))*u3 ) 
theta.hat.3 <- mean( g(x3)/f3(x3) )   # media di  (1-exp(-1)) / (1+x^2)
varianza.3 <-var(g(x3)/f3(x3)) / m  # varianza stimata di theta.hat
theta.hat.3

#CASO f4####
u4 <- runif(m)
x4 <- tan(pi/4*u4) 
theta.hat.4 <- mean( g(x4)/f4(x4) )   
varianza.4 <- var(  g(x4)/f4(x4)  ) / m  # varianza stimata di theta.hat
theta.hat.4

#confrontone####
# Compito: stimare la varianza dei diversi metodi
theta.hat <- c(theta.hat.1, theta.hat.2, theta.hat.3, theta.hat.4)
varianze <- c(varianza.1, varianza.2, varianza.3, varianza.4)

# Costruzione della matrice
matrice_dati <- matrix(data = c(theta.hat, varianze), ncol = 2, byrow = FALSE)

# Aggiunta dei nomi delle colonne
colnames(matrice_dati) <- c("theta.hat", "varianza")

print(matrice_dati)




