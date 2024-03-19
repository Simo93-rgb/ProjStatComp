# Definizione della funzione Cramér-von Mises con simulazione di permutazione
cvm_permutation_test <- function(sample_x, sample_y, n_permutations = 999, plotting = FALSE, save_plot = F ,name_plot="") {
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


  if(plotting){
    plotting_permutations(p_value = p_value,
                          W_squared_permutations = W_squared_tot,
                          W_squared_observed = W_squared_observed,
                          save_plot = save_plot,
                          name_plot = name_plot)
  }
  # Output dei risultati
  list(W_squared_observed = W_squared_observed, p_value = p_value)
}

library(ggplot2)

# plotting_permutations <- function(W_squared_permutations, W_squared_observed, save_plot = F, name_plot = "") {
#   #Visualizzazione dei risultati
#   hist(W_squared_permutations, breaks = 30, freq = F, col = 'skyblue', main = 'Distribuzione delle Statistiche di Permutazione', xlab = 'Statistiche di Cramer-von Mises')
#   abline(v = W_squared_observed, col = 'red', lwd = 2, lty = 2)
#   legend('topright', legend = c('Statistica Osservata', 'Permutazioni'), fill = c('red', 'skyblue'))
#
#
# }
plotting_permutations <- function(p_value, W_squared_permutations, W_squared_observed, save_plot = FALSE, name_plot = "") {
  df <- data.frame(W_squared = W_squared_permutations)
  p <- ggplot(df, aes(x = W_squared)) +
    geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
    geom_vline(xintercept = W_squared_observed, color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = "Distribuzione delle Statistiche di Permutazione", x = "Statistiche di Cramer-von Mises", y = "Frequenza") +
    theme_minimal()

  print(p) # Stampa il plot

  if(save_plot) {
    file_name <- if(name_plot != "") {
      paste0("10.3/plots/",name_plot, ".jpeg")
    } else {
      "10.3/plots/perm_cvm_plot.jpeg"
    }
    ggsave(file_name, plot = p, width = 7, height = 5, dpi = 320)
  }
}
# plotting_permutations <- function(W_squared_permutations, W_squared_observed) {
#   # Prepara i dati per ggplot2
#   data <- data.frame(W_squared = W_squared_permutations)
#
#   # Genera il plot
#   p <- ggplot(data, aes(x = W_squared)) +
#     geom_histogram(aes(y = ..density..), binwidth = diff(range(W_squared_permutations)) / 30, fill = "skyblue", color = "black") +
#     geom_vline(aes(xintercept = W_squared_observed), color = "red", linetype = "dashed", size = 1.5) +
#     theme_minimal() +
#     labs(title = "Distribuzione delle Statistiche di Permutazione",
#          x = "Statistiche di Cramer-von Mises",
#          y = "Densita'") +
#     scale_x_continuous(limits = c(min(W_squared_permutations), max(W_squared_permutations))) +
#     scale_y_continuous(limits = c(0, 1.05 * max(density(W_squared_permutations)$y)))
#
#   # Aggiunge una legenda manualmente
#   p <- p + annotate("text", x = W_squared_observed, y = 1.02 * max(density(W_squared_permutations)$y), label = "Statistica Osservata", color = "red", hjust = 0.5, size = 4) +
#     annotate("rect", xmin = min(W_squared_permutations), xmax = max(W_squared_permutations), ymin = 0, ymax = 1.05 * max(density(W_squared_permutations)$y), alpha = 0.1, fill = "skyblue") +
#     guides(fill=guide_legend(title="Legenda")) +
#     scale_fill_manual(name = "Tipo", values = c("Statistica Osservata" = "red", "Permutazioni" = "skyblue"), labels = c("Statistica Osservata", "Permutazioni"))
#
#   # Stampa il plot
#   print(p)
#   # Salva il plot
#   if(save_plot) {
#     # Costruisce il nome file con percorso
#     file_name <- if(name_plot != "") {
#       paste0("10.3/plots/", name_plot, ".jpeg")
#     } else {
#       "10.3/plots/perm_hist.jpeg"
#     }
#     ggsave(filename = file_name, plot = p, dpi = 320)
#   }
# }