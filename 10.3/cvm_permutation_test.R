# Definizione della funzione Cramér-von Mises con simulazione di permutazione
cvm_permutation_test <- function(sample_x, sample_y, n_permutations = 999) {

  # Calcolo della statistica W^2 osservata
  W_squared_observed <- cramer_von_mises(sample_x, sample_y)

  # Preparazione per la simulazione di permutazione
  combined_samples <- c(sample_x, sample_y)
  n_x <- length(sample_x)
  n_y <- length(sample_y)
  W_squared_permutations <- NULL

  # Esecuzione delle permutazioni e calcolo di W^2 per ciascuna
  for (i in 1:n_permutations) {
    # permuted_samples <- sample(combined_samples, replace = FALSE)
    permuted_samples <- sample((n_x+n_y),size = n_x, replace = FALSE)
    # permuted_x <- permuted_samples[1:n_x]
    permuted_x <- combined_samples[permuted_samples]
    # permuted_y <- permuted_samples[(n_x + 1):(n_x + n_y)]
    permuted_y <- combined_samples[-permuted_samples]
    W_squared_permutations[i] <- cramer_von_mises(permuted_x, permuted_y)
  }

  # Calcolo del p-value

  W_squared_tot <- c(W_squared_observed, W_squared_permutations)
  # non ho usato mean perché mi piace vedere una rappresentazione simile a quella
  # matematica vista a lezione
  p_value <- sum(W_squared_tot >= W_squared_observed) / (n_permutations + 1)

  # Visualizzazione dei risultati
  hist(W_squared_permutations, freq=FALSE, main = "Distribuzione di W^2 permutato",
       xlab = "W^2",  col = "skyblue", xlim = c(0, 10), ylim = c(0,1))
  abline(v = W_squared_observed, col = "red", lwd = 2)
  legend("topright", legend = c(paste("W^2 osservato =", round(W_squared_observed, 4)),
                                paste("p-value =", round(p_value, 4))),
         col = c("red", "black"), lwd = 2)

  # Output dei risultati
  list(W_squared_observed = W_squared_observed, p_value = p_value)
}