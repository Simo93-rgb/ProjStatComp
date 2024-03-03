# LAB 1
##

# questo è un commento


#uso R come calcolatrice####
# calcolo valore della densita' normale standard in 2
1/sqrt(2*pi) *exp(-2^2/2) 
dnorm(2)  # f.ne densita' normale 
pnorm(2) # prob. dell'evento (X<2)

#sequenza di valori, vettori, matrici, liste####

seq(0,1, 0.01)
x <- seq(0,1,0.01)

#array####
y <- 1:10 # vettore
dim(y)
dim(y) <- length(y)
dim(y)

#matrix####
matrix(y, nrow = 2) # riempie per colonna
A <- matrix(y, nrow = 2, byrow = TRUE) # riempie per riga
A[2,3]
A[,3]
A <- A* .5 
matrix(1:10, nrow=3, ncol = 5)

#nomi riservati di R####
TRUE
T
t(A) # trasposto
c(1,2,3,7,8) # costruisce un vettore concatenando i termini
pi # pi greco

#primi campionamenti####
# 
#####
Omega <- 1:6    # Spazio campionario
sample(Omega) # sample = campione 

sample(Omega, size = 10, replace = TRUE)

#scrivere una funzione####
### funzione per calcolare la somma dell'esito di n lanci di dado
somma.dadi <- function(n){
  # INPUT: n   numero di lanci del dado
  z <- sum(sample(1:6 , n, replace = TRUE))
  
  return(z)
}
somma.dadi(2)


#liste####
l <- list(A, Omega, somma.dadi)
l[[1]] # matrice A
l[[3]](1) # prende il terzo oggetto della lista (che e' una funzione) e la calcola in (1)

#####
# esempio di dataframe
#####
# particolare lista che raccoglie tutte le informazioni di un dataset
iris