# LAB-2
## 

#confrontiamo distribuzione t-student con N(0,1)####
xx <- seq(-3,3 , 0.01)
yy <- dt(xx, df = 5) #Crea t-student
plot(xx, yy, type = "l", col = 2, lwd = 3, ylim = c(0, 0.5))

yy.norm <- dnorm(xx)
lines(xx, yy.norm, col = 1, lwd = 3)

yy2 <- dt(xx, df = 2) #Crea t-student
lines(xx, yy2, col = 3, lwd = 3)

legend(-3, 0.3, legend = c("t-student", "N(0,1)", "t-student df=2"), 
       col = c(2,1,3), lwd = 3 )


#rappresentare la Legge dei grandi numeri (LGN)####
tempo_inizio <- system.time({
  Omega <- 1:6
  n <- 10000
  x <- sample(Omega, size = n, replace = TRUE)  # vettore di lunghezza "size" contenente i lanci del dado
  
  media <- cumsum(x)/1:n   # "cumsum" effettua somme cumulate e la divisione viene 
  # fatta componente per componente
  plot(media, xlab = "taglia campionaria", ylab = "media campionaria", 
       main = "media campionaria al variare di n",
       ylim = c(1,6) )
  abline(h=3.5, col = 2, lwd = 2)
})

print(tempo_inizio)



#rappresentare TLC####

n <- 40  # taglia delle X
m <- 10000 # taglia delle Z
x <- sample(1:6, size = m*n, replace = TRUE)
A <- matrix(x, ncol = n)  # contiene tutte le estrazioni fatte. dim = m x n .
dim(A)
medie.camp <- apply(A,1,mean) # applica la funzione "mean" a ciascuna riga della matrice A
length(medie.camp)
# dobbiamo standardizzare, serve calcolare media e varianza teorica:
media.teo <- 1:6 %*% rep(1/6, 6)  
# "rep" ripete il valore 1/6 in una sequenza con 6 elementi. In questo caso è il vettore delle probabilità
# %*%  calcola il prodotto scalare tra vettori

var.teo.0 <- (1:6 - media.teo)^2 %*% rep(1/6, 6) 
var.teo.1 <- (1:6 - rep(media.teo,6))^2 %*% rep(1/6, 6) 
var.teo.2 <- mean((1:6 - rep(media.teo,6))^2)
var.camp <- mean(x^2) - media.teo^2  

z <- (medie.camp - media.teo) / (var.teo.0 / sqrt(n))
length(z)
hist(z, freq = FALSE, breaks = 25)  # L'opzione freq = FALSE genera l'istogramma con le densita' di frequenza 
# (aree dei rettangoli possono essere interpretate come probabilita')
lines(xx, dnorm(xx), col = 2, lwd =2)
