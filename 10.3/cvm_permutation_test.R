library(ggplot2)
# Definizione della funzione Cramér-von Mises con simulazione di permutazione
cvm_permutation_test <- function(sample_x, sample_y, n_permutations = 999, plotting = FALSE, save_plot = F, name_plot = "") {
  # Calcolo della statistica W^2 osservata
  W_squared_observed <- cramer_von_mises(sample_x, sample_y, plotting = plotting)

  # Preparazione per la simulazione di permutazione
  combined_samples <- c(sample_x, sample_y)
  n_x <- length(sample_x)
  n_y <- length(sample_y)
  W_squared_permutations <- NULL

  # Esecuzione delle permutazioni e calcolo di W^2 per ciascuna
  for (i in 1:n_permutations) {
    permuted_samples <- sample(combined_samples, replace = FALSE)
    # permuted_samples <- sample((n_x+n_y),size = n_x, replace = FALSE)
    permuted_x <- permuted_samples[1:n_x]
    # permuted_x <- combined_samples[permuted_samples]
    permuted_y <- permuted_samples[(n_x + 1):(n_x + n_y)]
    # permuted_y <- combined_samples[-permuted_samples]
    W_squared_permutations[i] <- cramer_von_mises(permuted_x, permuted_y)
  }

  # Calcolo del p-value
  W_squared_tot <- c(W_squared_observed, W_squared_permutations)
  # non ho usato mean perché mi piace vedere una rappresentazione simile a quella
  # matematica vista a lezione
  p_value <- sum(W_squared_tot >= W_squared_observed) / (n_permutations + 1)


  if (plotting) {
    plotting_permutations(p_value = p_value,
                          W_squared_permutations = W_squared_tot,
                          W_squared_observed = W_squared_observed,
                          save_plot = save_plot,
                          name_plot = name_plot)
  }
  # Output dei risultati
  list(W_squared_observed = W_squared_observed, p_value = p_value)
}

plotting_permutations <- function(p_value, W_squared_permutations, W_squared_observed, save_plot = FALSE, name_plot = "") {
  df <- data.frame(W_squared = W_squared_permutations)
  p <- ggplot(df, aes(x = W_squared)) +
    geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
    geom_vline(xintercept = W_squared_observed, color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = "Distribuzione delle Statistiche di Permutazione", x = "Statistiche di Cramer-von Mises", y = "Frequenza") +
    theme_minimal()

  print(p) # Stampa il plot

  if (save_plot) {
    file_name <- if (name_plot != "") {
      paste0("10.3/plots/", name_plot, ".jpeg")
    } else {
      "10.3/plots/perm_cvm_plot.jpeg"
    }
    ggsave(file_name, plot = p, width = 7, height = 5, dpi = 320)
  }
}

