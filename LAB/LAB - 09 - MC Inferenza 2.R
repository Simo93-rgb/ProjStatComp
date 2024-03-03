## LABORATORIO 9
#

#esempio 7.7 - stima di alpha per test unilaterale destro####

m <- 1000
n <- 20
alpha <- 0.05
y <- replicate(m, expr = {
  x <- rnorm(n, mean = 500, sd = 4)
  # x <- rexp(n, rate = 1/500)
  t.oss <- (mean(x)-500)/sd(x)*sqrt(n)
  (t.oss > qt(1-alpha, df = n-1)  ) *1 
})
alpha.hat <- mean(y)
se.alpha.hat <- sqrt(alpha.hat *(1-alpha.hat) / m)
c(alpha, alpha.hat, se.alpha.hat)

#####
# esempio 7.9 - stima della potenza del test per la media unilaterale
#####
rm(list = ls())
m <- 1000 # taglia MC
n <- 20   # taglia dei j-mi campioni
mu0 <- 500  # il valore del parametro sotto H0
sigma <- 100 # serve per generare i dati
mu <- seq(450, 650, 10)  # vettore contenente i valori di H1
power <- NULL # inizializzo vettore contenente le potenze 
# stimate per ciascun mu
for (i in 1:length(mu)){
  # ripeto un MC per ciascun valore alternativo di mu
  y <- replicate(m, expr = {
    x <- rnorm(n, mean = mu[i], sd = sigma)  # j-mo campione sotto H1
    t.oss <- (mean(x)-mu0)/sd(x)*sqrt(n)   # j-ma stat.Test osservata
    (t.oss > qt(0.95, df = n-1))*1
  })
  # y <- NULL
  # for (j in (1:m)){
  #   x <- rnorm(n, mean = mu[i], sd = sigma)  # j-mo campione sotto H1
  #   t.oss <- (mean(x)-mu0)/sd(x)*sqrt(n)   # j-ma stat.Test osservata
  #   y[j] <- (t.oss > qt(0.95, df = n-1))*1
  # }
  power[i] <- mean(y)  # potenza stimata per la i-ma ipotesi alternativa
}

plot(mu , power, xlab = "mu", ylab = "power")
abline(v = mu0, col = 2)
abline(h=0.05, col = 3)

#####
# Esempio di uso del t.test per test per la media:
#####
lambda <- 1
x <- rexp(100, rate = lambda)
mu.teo <- 1/lambda
# salvo l'esito del test in una variabile ausiliare 
# per poi estrarne i valori calcolati
aus <- t.test(x, alternative = "two.sided", mu = mu.teo)
aus
aus$parameter
aus$statistic
aus$p.value
