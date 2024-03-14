#10.2 Cramér-Von Mises

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

# Caricamento dataset dei pulcini
attach(chickwts)
boxplot(formula(chickwts))
soybean <- sort(as.vector(weight[feed == "soybean"]))
casein <- sort(as.vector(weight[feed == "casein"]))
linseed <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

# definito molto elevato immaginando uno studio esplorativo che mi permette 
# tolleranza agli errori del primo tipo, anche detti falsi negativi
alpha <- 0.18

# distanza fra soybean e linseed
res_s_l <- cramer_von_mises(soybean, linseed)

# distanza fra soybean e casein
res_s_c <- cramer_von_mises(soybean, casein)

# distanza fra linseed e casein
res_l_c <- cramer_von_mises(linseed, casein)


if(res_s_l <= alpha){
  print("soybean e linseed -> Accetto al mia ipotesi H0: CDF(S) = CDF(L)")
} else {
  print("soybean e linseed -> Rifiuto al mia ipotesi H0: CDF(S) = CDF(L) 
        accetto H1:CDF(S) != CDF(L)")
} 

if (res_s_c <= alpha){
  print("soybean e casein -> Accetto al mia ipotesi H0: CDF(S) = CDF(C)")
}else {
  print("soybean e casein -> Rifiuto al mia ipotesi H0: CDF(S) = CDF(C) 
        accetto H1:CDF(S) != CDF(C)")
} 

if (res_l_c <= alpha){
  print("linseed e casein -> Accetto al mia ipotesi H0: CDF(L) = CDF(C)")
}else {
  print("linseed e casein -> Rifiuto al mia ipotesi H0: CDF(L) = CDF(C) 
        accetto H1:CDF(L) != CDF(C)")
} 
