## LAB 11
####

#esempio 8.6####

library(bootstrap)
data("patch")
patch

# z <- c(8406, 2342, 8187, 8459, 4795, 3516, 4796, 10238)
z <- patch$z
#y <- c(-1200, 2601, -2705, 1982, -1290, 351, -638, -2719)
y <- patch$y
patch <- data.frame(cbind(z,y))

n <- nrow(patch)
theta.hat <- mean(patch$y)/mean(patch$z) # theta stimato sul campione
theta.b <- NULL # vettore per accumulare le repliche boostrap 
B <- 2000

for (b in 1:B){
  i <- sample(1:n, size =n, replace = TRUE)
  y_b <- patch$y[i]
  z_b <- patch$z[i]
  theta.b[b] <- mean(y_b) / mean(z_b)
}

hist(theta.b) # stima distribuzione boostrap di rho.hat
bias.b <- mean(theta.b) - theta.hat
se.b <- sd(theta.b)

## stima JK del bias e dell'errore standard
theta.jk <- NULL
for (i in 1:n){
  theta.jk[i] <- mean(y[-i]) / mean(z[-i])
}
bias.jk <- (n - 1) * (mean(theta.jk) - theta.hat)
se.jk <- sqrt((n-1)*mean((theta.jk - mean(theta.jk))^2))
cbind(theta.hat, bias.b, bias.jk, se.b, se.jk)

#JK fallisce####

n <- 10
x <- sample(1:100, size = n)
median(x)

# stima JK dello standard error
M <- NULL
for (i in 1:n) {
  y <- x[-i]
  M[i] <- median(y)
}
se.jk <- sqrt( (n-1) * mean((M - mean(M))^2))

#bootstrap estimate of se
B <- 1000
M.b <- NULL
for (b in 1:B){
  y <- sample(x, size = n, replace = TRUE)
  M.b[b] <- median(y) 
}
se.b <- sd(M.b)

cbind(se.jk, se.b)

# x
# head(M)
# head(M.b)


#Esempio 10.1 test di permutazione########

attach(chickwts)
boxplot(formula(chickwts))
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

x
y
###
t.test(x,y)  # test sulle medie dei due gruppi (distribuzioni assunte gaussiane)
theta.hat <- t.test(x,y)$statistic
###

R <- 999 #numero di repliche + quello osservato = 1000
z <- c(x, y) #campione congiunto
K <- 1:length(z) # indici del campione congiunto che uso per le permutazioni
repliche.test <- NULL
theta.hat <- t.test(x, y)$statistic  # valore stat.test osservato sul campione originale
pv.oss <- t.test(x, y)$p.value # p.value osservato per il test
for (i in 1:R) {
  # indici per il nuovo vettore x
  k <- sample(K, size = length(x), replace = FALSE)
  x1 <- z[k]  # costruisco il vettore x dal vettore congiunto z
  y1 <- z[-k] # vettore complementare di x in z
  # valore stat.test sul campione permutato:
  repliche.test[i] <- t.test(x1, y1)$statistic 
}
p <- mean(c(theta.hat, repliche.test) >= theta.hat)

hist(repliche.test, main = "", freq = FALSE, 
     xlab = paste(c("T (p = ", 2*p ," )"), collapse = ""), breaks = 20)
points(theta.hat, 0, cex = 1, pch = 16) # valore osservato della statistica

