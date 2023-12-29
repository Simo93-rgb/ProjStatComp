# Allenamenti pre esame

#Esercizio 7.2####
# Plot the empirical power curve for the t-test in Example 7.9, changing
# the alternative hypothesis to H1 : μ6 = 500, and keeping the significance
# level α = 0.05.

n <- 20
m <- 1000
mu0 <- 500
sigma <- 100
mu <- c(seq(450, 650, 10)) #alternatives
M <- length(mu)
power <- numeric(M)

espressione <- function(n, mu0, mu1){
  #simulate under alternative mu1
  x <- rnorm(n, mean = mu1, sd = sigma)
  ttest <- t.test(x, alternative = "greater", mu = mu0)
  return(ttest$p.value) # il dollaro accede al valore p.value 
}

for (i in 1:M) {
  mu1 <- mu[i]
  pvalues <- replicate(m, expr =  espressione(n, mu0, mu1))
  power[i] <- mean(pvalues <= .05)
}
se <- sqrt(power * (1-power) / m)

library(ggplot2)
df <- data.frame(mean=mu, power=power,
                 upper=power+2*se, lower=power-2*se)
ggplot(df, aes(x=mean, y=power)) +
  geom_line() +
  geom_vline(xintercept=500, lty=2) +
  geom_hline(yintercept=c(0,.05), lty=1:2) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width = 0.2, lwd=1.5)
