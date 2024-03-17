cramer_von_mises <- function(sample_x, sample_y) {
  # Lunghezza del campione X
  n <- length(sample_x)
  # Lunghezza del campione Y
  m <- length(sample_y)

  # Creazione del rango di X e di Y
  # Per fare i ranghi si guarda la posizione dei valori di un vettore ordinato
  # ma questo è temporaneo, non va a cambiare la posizione originale degli
  # elementi. Quindi, io so che i primi n elementi sono tutti i ranghi di X e
  # gli ultimi m elementi sono tutti elementi di Y
  combinati <- c(sample_x, sample_y)
  ranked_sample <- rank(combinati)
  rank_x <- ranked_sample[1:n]
  rank_y <- ranked_sample[(n+1):(n+m)]


  # Rappresenta la differenza fra i ranghi osservati e quelli teorici
  U <- n * sum( (rank_x-(1:n))^2 ) + m * sum( (rank_y-(1:m))^2 )

  # Distanza delle distribuzioni
  W_squared <- U / ( n*m*(n+m) ) - ( (4*n*m) -1 ) / ( 6*(n+m) )

  return (W_squared)
}