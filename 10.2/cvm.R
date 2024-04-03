library(ggplot2)
cramer_von_mises <- function(sample_x, sample_y, plotting = FALSE, save_plot = F, name_plot = "") {
  # Siccome non ho garanzia che i campini siano ordinati la prima cosa
  # che faccio è un sort
  sample_x <- sort(as.vector(sample_x))
  sample_y <- sort(as.vector(sample_y))

  # Lunghezza del campione X
  n <- length(sample_x)
  # Lunghezza del campione Y
  m <- length(sample_y)

  # unisco i campioni
  combinati <- c(sample_x, sample_y)
  # faccio un rango congiunto
  ranked_sample <- rank(combinati, ties.method = "average")
  # separo i ranghi
  rank_x <- ranked_sample[1:n]
  rank_y <- ranked_sample[(n + 1):(n + m)]

  # Scommentare se serve parlare dei ranghi
  # print(cbind(sample_x, rank_x,sample_y, rank_y))

  U <- n * sum((rank_x - (1:n))^2) + m * sum((rank_y - (1:m))^2)

  # Distanza delle distribuzioni
  W_squared <- U / (n * m * (n + m)) - ((4 * n * m) - 1) / (6 * (n + m))

  if (plotting == T) {
    plotting_data_cvm(sample_x, sample_y, name_plot = name_plot, save_plot = save_plot)
  }
  return(W_squared)
}

plotting_data_cvm <- function(sample_x, sample_y, save_plot = FALSE, name_plot = "") {
  n <- length(sample_x)
  m <- length(sample_y)

  # Costruisce il plot ECDF utilizzando ggplot2
  p <- ggplot(data.frame(Valori = c(sample_x, sample_y), Gruppo = rep(c('X', 'Y'), c(n, m))),
              aes(x = Valori, colour = Gruppo)) +
    stat_ecdf() +
    theme_minimal() +
    labs(title = "ECDF dei Campioni", x = "Valori", y = "F(x)") +
    scale_color_manual(values = c('X' = 'blue', 'Y' = 'red')) # Specifica manualmente i colori

  # Stampa il plot
  print(p)
  # Salva il plot
  if (save_plot) {
    # Costruisce il nome file con percorso
    file_name <- if (name_plot != "") {
      paste0("10.2/plots/", name_plot, ".jpeg")
    } else {
      "10.2/plots/eCDF.jpeg"
    }
    ggsave(filename = file_name, plot = p, dpi = 320)
  }
}