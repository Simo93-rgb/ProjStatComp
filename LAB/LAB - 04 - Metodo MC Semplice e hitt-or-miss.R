# LAB-4
## 


#Esempio 1: Esponenziale####
# 1. Genero un insieme di uniformi iid come U(0,1)
taglia.campionaria <- 1000
# uniformi <- runif(taglia.campionaria)
uniformi <- sample(seq(0,1, by=0.000001),taglia.campionaria,replace = T)
# 2. definisco theta come la soluzione analitica dell'integrale in [0,1]
theta <- 1-exp(-1)
# 3. calcolo theta cappello. 
theta.hat.sum <- taglia.campionaria^-1 * sum(exp(-uniformi))
theta.hat.mean <- mean(exp(-uniformi))
# Quì per curiosità ho calcolato sia come sommatoria che come media
c(theta, theta.hat.mean)
# 4. Confronto grafico
theta.hat.mean <- cumsum(exp(-uniformi))/(1:taglia.campionaria)
# cumsum() mi permette di vedere l'andamento man mano che procedo
# con l'esperimento, altrimenti avrei un plot con un punto solo
plot(theta.hat.mean)
abline(h=theta, col=2, lwd=2)


#Esempio 2: uniforme(2,2)####

rm(list = ls()) # svuoto environment dalle variabili
theta <- exp(-2) - exp(-4) # valore teorico
m <- 1000
u <- runif(m, min = 2, max = 4) # 2+2*runif(m)
theta.hat <- 2*mean(exp(-u))
c(theta, theta.hat)

# per confronto grafico devo tener traccia di tutte le medie
theta.hat <- 2*cumsum(exp(-u))/(1:m)
plot(theta.hat)
abline(h=theta, col = 2, lwd = 2)




#Esempio 3.1: intervallo integrazione illimitato####
rm(list = ls())
x <- seq(.1, 2.5, 0.1)
m <- 1000
theta.hat.1 <- NULL
for (i in 1:length(x)){
  u <- runif(m, min = 0, max = x[i])
  theta.hat.1[i] <- 0.5 + x[i]*mean( 1/sqrt(2*pi) * exp(-u^2/2) )
}
# theta.hat è cdf empirica
cdf.teo <- pnorm(x)

se.theta.hat.1 <- sqrt(theta.hat.1 * (1-theta.hat.1) / m)
cbind(x,cdf.teo, theta.hat.1, se.theta.hat.1)




#Esempio 3.2 (approccio hit-or-miss)####

x <- seq(.1, 2.5, 0.1)
m <- 1000
theta.hat.2 <- NULL
for (i in 1:length(x)){
  z <- rnorm(m)
  theta.hat.2[i] <- mean(z <= x[i]) 
}
cdf.teo <- pnorm(x)
se.theta.hat.2 <- sqrt(theta.hat.2 * (1-theta.hat.2) / m)
cbind(x,cdf.teo, theta.hat.1, se.theta.hat.1, theta.hat.2, se.theta.hat.2)
stime <- cbind(x,cdf.teo, theta.hat.1, se.theta.hat.1, theta.hat.2, se.theta.hat.2)

# costruisco IC a livello 1-alpha per MC semplice 1:
alpha <- 0.1
estr.inf <- theta.hat.1 - qnorm(1-alpha/2) * se.theta.hat.1
estr.sup <- theta.hat.1 + qnorm(1-alpha/2) * se.theta.hat.1
cbind(estr.inf, cdf.teo,  estr.sup)


#Esempio 3.3 (metodo variabili antitetiche)####

x <- seq(.1, 2.5, .1)
m <- 1000
theta.hat.1.ant <- Y <- Y.ant <- NULL
for (i in 1:length(x)){
  u <- runif(m/2)
  Y <- 0.5 + (x[i]/sqrt(2*pi) * exp(-(u*x[i])^2/2))
  Y.ant <- 0.5 + (x[i]/sqrt(2*pi) * exp(-((1-u)*x[i])^2/2))
  theta.hat.1.ant[i] <- mean(c(Y, Y.ant))
}
# theta.hat è cdf empirica
cdf.teo <- pnorm(x)

# prossima riga e' sbagliata. perche'?
se.theta.hat.1.ant <- sqrt(theta.hat.1.ant * (1-theta.hat.1.ant) / m) # NO
cbind(x,cdf.teo, theta.hat.1, se.theta.hat.1, theta.hat.1.ant, se.theta.hat.1.ant)


# ATT: non posso usare la formula del caso di indipendenza!
# per stimare lo standard error ripeto tante volte MC con var.antitetiche e calcolo
# la dev.standard delle stime ottenute

x <- seq(.1, 2.5, .1)
m <- 1000
N <- 100
theta.hat.1.ant <- matrix(0,nrow = N, ncol = length(x))
Y <- Y.ant <- NULL
for (j in 1:N){
  for (i in 1:length(x)){
    u <- runif(m/2)
    Y <- 0.5 + (x[i]/sqrt(2*pi) * exp(-(u*x[i])^2/2))
    Y.ant <- 0.5 + (x[i]/sqrt(2*pi) * exp(-((1-u)*x[i])^2/2))
    theta.hat.1.ant[j,i] <- mean(c(Y, Y.ant))
  }
}
se.theta.hat.1.ant <- apply(theta.hat.1.ant, 2, sd)
cbind(x,cdf.teo, theta.hat.1, se.theta.hat.1, se.theta.hat.1.ant)


#versione alternativa per il confronto dello standard error (libro del Rizzo)####


MC.phi <- function(x, R=1000, antitetica = TRUE){
  u <- runif(R/2) 
  # la rimanente metà del campione
  if (antitetica){   # se c'èuna sola istruzione la graffa può essere evitata
    v <- 1-u # CASO ANTITETICA
  } else {
    v <- runif(R/2) # CASO MC DIRETTO
  }
  u <- c(u,v)
  
  cdf <- NULL
  for (i in 1:length(x)){
    g <- 0.5 + (x[i]/sqrt(2*pi) * exp(-(u*x[i])^2/2))
    cdf[i] <- mean(g)
  }
  return(cdf)
}

x <- seq(.1, 2.5, length=10)
Phi.teo <- pnorm(x)
set.seed(123)
MC.diretto <- MC.phi(x, antitetica = FALSE)
set.seed(123)
MC.ant <- MC.phi(x, antitetica = TRUE)

round(cbind(x, Phi.teo, MC.diretto, MC.ant), 6)


#Misuro errore standard dei due metodi####
N <- 100  # num. di volte che ripeto la stima MC
x <- 2
MC.dir <- MC.ant <- NULL
for (j in 1:N){
  MC.dir[j] <- MC.phi(x, R=1000, antitetica = FALSE)
  MC.ant[j] <- MC.phi(x, R=1000)
  
}
sd(MC.dir)
sd(MC.ant)
(var(MC.dir)-var(MC.ant))/var(MC.dir)  # differenza in termini relativi dei due metodi
