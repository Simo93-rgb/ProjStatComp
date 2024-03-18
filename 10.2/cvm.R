cramer_von_mises <- function(sample_x, sample_y) {
  sample_x <- sort(as.vector(sample_x))
  sample_y <- sort(as.vector(sample_y))
  # Lunghezza del campione X
  n <- length(sample_x)
  # Lunghezza del campione Y
  m <- length(sample_y)

  # unisco i campioni
  combinati <- c(sample_x, sample_y)
  # faccio un rango congiunto
  ranked_sample <- rank(combinati)
  # separo i ranghi
  rank_x <- ranked_sample[1:n]
  rank_y <- ranked_sample[(n+1):(n+m)]

  # Scommentare se serve parlare dei ranghi
  # print(cbind(sample_x, rank_x,sample_y, rank_y))

  # Rappresenta la differenza fra i ranghi osservati e quelli teorici
  U <- n * sum( (rank_x-(1:n))^2 ) + m * sum( (rank_y-(1:m))^2 )

  # Distanza delle distribuzioni
  W_squared <- U / ( n*m*(n+m) ) - ( (4*n*m) -1 ) / ( 6*(n+m) )

  return (W_squared)
}

