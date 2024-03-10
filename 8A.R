# Progetto 8A

#gpt-4####
# Carica le librerie necessarie
library(boot)

# Funzione per calcolare la media di un campione
media_campione <- function(dati, indici) {
  return(mean(dati[indici]))
}

# Inizializza i parametri dello studio Monte Carlo
numero_simulazioni <- 1000
dimensione_campione <- 200
media_vera <- 0
deviazione_standard <- 1

# Inizializza i contatori per le mancanze degli intervalli di confidenza
mancanze_sinistra_normale <- mancanze_destra_normale <- 0
mancanze_sinistra_base <- mancanze_destra_base <- 0
mancanze_sinistra_percentile <- mancanze_destra_percentile <- 0

# Ciclo Monte Carlo
for(i in 1:numero_simulazioni) {
  # Genera un campione dalla distribuzione normale
  campione <- rnorm(dimensione_campione, media_vera, deviazione_standard)
  # Calcola gli intervalli di confidenza bootstrap
  risultato_bootstrap <- boot(campione, media_campione, R = 1000)
  
  # Calcola l'intervallo di confidenza bootstrap normale standard
  ic_normale <- boot.ci(risultato_bootstrap, type = "norm")
  
  # Calcola l'intervallo di confidenza bootstrap di base
  ic_base <- boot.ci(risultato_bootstrap, type = "basic")
  
  # Calcola l'intervallo di confidenza percentile
  ic_percentile <- boot.ci(risultato_bootstrap, type = "perc")
  
  # Controlla le mancanze per l'intervallo di confidenza normale
  if(media_vera < ic_normale$normal[2]) mancanze_sinistra_normale <- mancanze_sinistra_normale + 1
  if(media_vera > ic_normale$normal[3]) mancanze_destra_normale <- mancanze_destra_normale + 1
  
  # Controlla le mancanze per l'intervallo di confidenza di base
  if(media_vera < ic_base$basic[4]) mancanze_sinistra_base <- mancanze_sinistra_base + 1
  if(media_vera > ic_base$basic[5]) mancanze_destra_base <- mancanze_destra_base + 1
  
  # Controlla le mancanze per l'intervallo di confidenza percentile
  if(media_vera < ic_percentile$perc[4]) mancanze_sinistra_percentile <- mancanze_sinistra_percentile + 1
  if(media_vera > ic_percentile$perc[5]) mancanze_destra_percentile <- mancanze_destra_percentile + 1
}

# Calcola le proporzioni di mancanze
proporzione_mancanze_sinistra_normale <- mancanze_sinistra_normale / numero_simulazioni
proporzione_mancanze_destra_normale <- mancanze_destra_normale / numero_simulazioni
proporzione_mancanze_sinistra_base <- mancanze_sinistra_base / numero_simulazioni
proporzione_mancanze_destra_base <- mancanze_destra_base / numero_simulazioni
proporzione_mancanze_sinistra_percentile <- mancanze_sinistra_percentile / numero_simulazioni
proporzione_mancanze_destra_percentile <- mancanze_destra_percentile / numero_simulazioni

# Stampa i risultati
cat("Proporzioni di mancanze per l'intervallo di confidenza bootstrap normale standard: sinistra", proporzione_mancanze_sinistra_normale, "destra", proporzione_mancanze_destra_normale, "\n")
cat("Proporzioni di mancanze per l'intervallo di confidenza bootstrap di base: sinistra", proporzione_mancanze_sinistra_base, "destra", proporzione_mancanze_destra_base, "\n")
cat("Proporzioni di mancanze per l'intervallo di confidenza percentile: sinistra", proporzione_mancanze_sinistra_percentile, "destra", proporzione_mancanze_destra_percentile, "\n")
