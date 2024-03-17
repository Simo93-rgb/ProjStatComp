#10.2 con p-value####
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
  ranked_sample <- rank(c(sample_x, sample_y))
  rank_x <- ranked_sample[1:n]
  rank_y <- ranked_sample[n+1:m]
  
  
  # Richiesta computazione dall'esercizio 
  # Rappresenta la differenza fra i ranghi osservati e quelli teorici
  # Ci mostra la discrepanza che c'è fra le 
  U <- n * sum( (rank_x-(1:n))^2 ) + m * sum( (rank_y-(1:m))^2 )
  
  # Distanza delle distribuzioni
  W_squared <- U / ( n*m*(n+m) ) - ( (4*n*m) -1 ) / ( 6*(n+m) ) 
  
  return (W_squared)
}

# Funzione di simulazione per calcolare il p-value
simulate_p_value <- function(sample_x, sample_y, observed_stat, n_simulations = 999) {
  combined_samples <- c(sample_x, sample_y)
  n_x <- length(sample_x)
  n_y <- length(sample_y)
  
  greater_count <- 0
  
  for (i in 1:n_simulations) {
    permuted_samples <- sample(combined_samples)
    permuted_x <- permuted_samples[1:n_x]
    permuted_y <- permuted_samples[(n_x + 1):(n_x + n_y)]
    
    W_squared_permuted <- cramer_von_mises(permuted_x, permuted_y)
    
    if (W_squared_permuted > observed_stat) {
      greater_count <- greater_count + 1
    }
  }
  
  p_value <- greater_count / n_simulations
  return(p_value)
}

# Caricamento del dataset PlantGrowth
attach(PlantGrowth)
boxplot(formula(PlantGrowth))

# Suddivisione del dataset in base ai gruppi di trattamento
ctrl_group <- PlantGrowth$weight[PlantGrowth$group == 'ctrl']
trt1_group <- PlantGrowth$weight[PlantGrowth$group == 'trt1']
trt2_group <- PlantGrowth$weight[PlantGrowth$group == 'trt2']

detach(PlantGrowth)

# Calcola W^2 osservato
W_squared_observed <- cramer_von_mises(ctrl_group, trt1_group)

# Calcola il p-value
p_value <- simulate_p_value(ctrl_group, trt1_group, W_squared_observed, 1000)

# Stampa il p-value
cat("Il p-value per il confronto tra 'ctrl' e 'trt1' è:", p_value, "\n")

# Decisione basata su alpha
alpha <- 0.05
if (p_value < alpha) {
  cat("Rifiutiamo l'ipotesi nulla: esiste una differenza significativa tra i gruppi.\n")
} else {
  cat("Non rifiutiamo l'ipotesi nulla: non ci sono prove sufficienti di una differenza significativa.\n")
}



#10.2 p-value con funzioni####
# Definizione della funzione per calcolare la statistica di Cramér-von Mises
cramer_von_mises <- function(sample_x, sample_y) {
  n <- length(sample_x)
  m <- length(sample_y)
  ranked_sample <- rank(c(sample_x, sample_y))
  rank_x <- ranked_sample[1:n]
  rank_y <- ranked_sample[(n + 1):(n + m)]
  
  U <- n * sum((rank_x - (1:n))^2) + m * sum((rank_y - (1:m))^2)
  W_squared <- U / (n * m * (n + m)) - ((4 * n * m) - 1) / (6 * (n + m))
  
  return(W_squared)
}

# Definizione della funzione per calcolare il p-value attraverso la simulazione di permutazione
calculate_p_value_cvm <- function(sample_x, sample_y, n_simulations, dataset_name) {
  W_squared_observed <- cramer_von_mises(sample_x, sample_y)
  print("W^2 = ")
  print(W_squared_observed)
  combined_samples <- c(sample_x, sample_y)
  n_combined <- length(combined_samples)
  
  replicate_stats <- replicate(n_simulations, {
    k <- sample(n_combined, size=length(sample_x))
    sample_x_from_combined_samples <- combined_samples[k]
    sample_y_from_combined_samples <- combined_samples[-k]
    cramer_von_mises(sample_x_from_combined_samples, sample_y_from_combined_samples)
  })
  print(replicate_stats)
  p_value <- mean(c(W_squared_observed, replicate_stats) >= W_squared_observed)
  
  # Visualizzazione dei risultati
  hist(replicate_stats, main = dataset_name, freq = FALSE,
       xlab = paste("W^2 (p =", round(p_value, 4), ")"), breaks = 20)
  abline(v = W_squared_observed, col = 'red', lwd = 2)
  legend("topright", legend = paste("W^2 osservato =", round(W_squared_observed, 4)), col = 'red', lwd = 2)
  
  # Decisione basata sul p-value
  alpha <- 0.05
  if (p_value < alpha) {
    message("Rifiutiamo l'ipotesi nulla: esiste una differenza significativa tra i gruppi.\n")
  } else {
    message("Non rifiutiamo l'ipotesi nulla: non ci sono prove sufficienti di una differenza significativa.\n")
  }
  
  return(p_value)
}

# Esempio di utilizzo della funzione con il dataset PlantGrowth
attach(PlantGrowth)
sample_x <- PlantGrowth$weight[PlantGrowth$group == 'ctrl']
sample_y <- PlantGrowth$weight[PlantGrowth$group == 'trt1']
detach(PlantGrowth)
# Calcola e visualizza il p-value per il confronto tra 'ctrl' e 'trt1'
p_value <- calculate_p_value_cvm(sample_x, sample_y, 999, "PlantGrowth")

attach(chickwts)
boxplot(formula(chickwts))
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
p_value <- calculate_p_value_cvm(x, y, 10, "chickwts")
p_value_obs <- t.test(x,y)$p.value
