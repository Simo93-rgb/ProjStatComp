# LAB-3
## 

#i numeri generati sono PSEUDO-casuali####

set.seed(123)
runif(1)


#METODO di inversione PER GENERARE VARIABILI CASUALI####
# generiamo esponenziale di parametro lambda
lambda <- 0.1
n <- 1000
u <- runif(n)
x <- -1/lambda * log(1-u)
hist(x, freq = F)
xx <- seq(0.01,100, 0.01)
lines(xx, dexp(xx, rate = lambda), col = 2, lwd = 2)

rexp(100, rate = lambda) # generare variabili casuali esponenziali usando R

#genero Bernoulli di parametro pi.teo####
pi.teo <- 0.3
(runif(100) < pi.teo)*1  # esiti di bernoulli di parametro 0.3

# generica v.c. discreta
poss.esiti <- c(1,3,6,7)
prob.associate <- c(.1, .3, .2, .4)
sample(poss.esiti,size = 20, replace = TRUE, prob = prob.associate)

#METODO del rigetto PER GENERARE VARIABILI CASUALI####

# generare una beta(2,2) con il metodo del rigetto 

n <- 10000 # taglia del campione di X
k <- 0 # contatore per monitorare il ciclo while
k.2 <- 0 # conta quanti cicli sono stati effettuati

g <- function(y){ # densita' di Y cioe' uniforme
  1*(y>0)*(y<1)
}

f <- function(y){ # densita' di X cioe' Beta(2,2)
  6*y*(1-y)*(y>0)*(y<1)
}

c <- 1.5 # sup(f/g)

x <- NULL  # inizializzo vettore contenente il campione di X
# x <- numeric(n)  # inizializzazione alternativa

# fintanto che non ho un campione di taglia n non posso 
# interrompere il metodo del rigetto
while (k<n){
  k.2 <- k.2+1
  y <- runif(1)
  u <- runif(1)
  if (u < f(y)/(c*g(y))){
    k <- k+1  # incremento di 1 il numero di punti accettati
    x <- c(x, y)  # concatena i punti gia' accettati per X con il nuovo punto
    ## x[k] <- y
  }
}

# confronto qualitativo tra distribuzione empirica della X simulata e una beta(2,2)
hist(x, freq = FALSE, breaks = 20)
xx <- seq(0,1,0.01)
lines(xx, dbeta(xx, 2,2), col = 2, lwd=2)

# confronto dei quantili
p <- seq(.1, .9, .05)
q.emp <- quantile(x, p)  # quantili empirici
q.teo <- qbeta(p, 2,2) # quantili teorici della beta(2,2)
rbind(q.emp, q.teo) # lego i vettori precedenti per riga
# qqplot
plot(q.emp, q.teo)
abline(0,1) # bisettrice primo e terzo quadrante

#METODO della convoluzione o somma di v.c.####

# Genero una somma (o convoluzione) di bernoulli
pi.teo <- 0.5 # parametro binomiale
n <- 10 # taglia del campione di Bernoulli (secondo parametro della binomiale)
bin <- sum(runif(n)<pi.teo)  # simulo 1 binomiale di par. pi.teo e n
bin

m <- 1000 # taglia del campione di binomiali
B <- (matrix(runif(n*m), nrow = m) < pi.teo)*1
head(B)
tail(B)
x <- apply(B, 1, sum )
x

barplot(table(x))
table(x)
# table() calcola la distribuzione di frequenze empirica
# barplot() diagramma a barre della distribuzione
plot(table(x)/m)
points(0:10, dbinom(0:10, n, pi.teo), col = 2)

#Generare una mistura di Gaussiane####

rm(list = ls()) # rimuove tutti gli oggetti del workspace
n <- 1000 # taglia della mistura
lambda <- 0.2  # peso della prima componente della mistura (N(0,1))

n1 <- round(n*lambda, digits = 0) # numerosita' del primo gruppo
n2 <- n - n1  # numerosita' del secondo gruppo

x <- c( rnorm(n1, 0,1) , rnorm(n2, 5, 1) )
hist(x, freq = FALSE, breaks = 20)
xx <- seq(-3, 9, 0.01)
lines(xx, lambda*dnorm(xx)+(1-lambda)*dnorm(xx,5,1), col = 2, lwd = 2)


#Generare una distribuzione multivariata normale####

install.packages("mtvnorm") # per installare una libreria aggiuntiva
library(mvtnorm) # per caricare la libreria
library(MASS)

mu <- c(2,3)
sigma <- matrix( c(4, 5, 5, 9), nrow =2   )
x <- mvrnorm(1000, mu, sigma)
plot(x)
