# import della funzione Cramér-Von Mises
source("10.2/cvm.R")
source("10.3/cvm_permutation_test.R")
source("Datasets/plantGrowth.R")
source("Datasets/chickwts.R")


# Esecuzione del test di permutazione Cramér-von Mises
result <- cvm_permutation_test(linseed, linseed)
result <- cvm_permutation_test(linseed, soybean,
                               plotting = T)
result <- cvm_permutation_test(trt1_group, trt2_group)

# Stampa del risultato
cat("W^2 osservato:", result$W_squared_observed, "\n")
cat("p-value:", result$p_value, "\n")
