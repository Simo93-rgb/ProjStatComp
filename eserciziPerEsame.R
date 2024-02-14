# Allenamenti pre esame

# Esercizio 7.2####
# Plot the empirical power curve for the t-test in Example 7.9, changing
# the alternative hypothesis to H1 : μ6 = 500, and keeping the significance
# level α = 0.05.

n <- 20 # Numero di osservazioni in ogni campione
m <- 1000 # Numero di simulazioni per stima della potenza
mu0 <- 500 # Media sotto l'ipotesi nulla
sigma <- 100 # Deviazione standard dei campioni
mu <- c(seq(450, 650, 10)) # Vettore di medie alternative da testare
M <- length(mu) # Numero di medie alternative
power <- numeric(M) # Inizializzazione del vettore per la potenza

espressione <- function(n, mu0, mu1){
  # Simula sotto l'ipotesi alternativa mu1
  x <- rnorm(n, mean = mu1, sd = sigma) # Genera campione casuale
  ttest <- t.test(x, alternative = "greater", mu = mu0) # Esegue il test t
  return(ttest$p.value) # Restituisce il valore p del test t
}

for (i in 1:M) {
  mu1 <- mu[i] # Seleziona la media alternativa corrente
  pvalues <- replicate(m, expr = espressione(n, mu0, mu1)) # Simula m test t
  power[i] <- mean(pvalues <= .05) # Calcola la potenza come proporzione di test significativi
}
se <- sqrt(power * (1-power) / m) # Calcola l'errore standard della potenza

# Graficazione
library(ggplot2)
df <- data.frame(mean=mu, power=power,
                 upper=power+2*se, lower=power-2*se) # Crea un dataframe per il grafico
ggplot(df, aes(x=mean, y=power)) +
  geom_line() + # Linea della potenza
  geom_vline(xintercept=500, lty=2) + # Linea verticale per μ0
  geom_hline(yintercept=c(0,.05), lty=1:2) + # Linee orizzontali per riferimenti
  geom_errorbar(aes(ymin=lower, ymax=upper), width = 0.2, lwd=1.5) # Barre di errore

