# import della funzione Cramér-Von Mises
source("10.2/cvm.R")
source("10.3/cvm_permutation_test.R")
source("Datasets/plantGrowth.R")
source("Datasets/chickwts.R")


# Esecuzione del test di permutazione Cramér-von Mises
result_l_l <- cvm_permutation_test(linseed, linseed)
result_l_s <- cvm_permutation_test(linseed, soybean,
                                   plotting = T,
                                   save_plot = T,
                                   name_plot = "chickwts_linseed_soybean")
result_l_c <- cvm_permutation_test(linseed, casein, plotting = T)
result_l_s <- cvm_permutation_test(linseed, sunflower, plotting = T)
result_ctrl_trt1 <- cvm_permutation_test(ctrl_group, trt1_group,
                                         plotting = T,
                                         save_plot = T,
                                         name_plot = "plant_ctrl_trt1")
result_ctrl_trt2 <- cvm_permutation_test(ctrl_group, trt2_group,
                                         plotting = T,
                                         save_plot = T,
                                         name_plot = "plant_ctrl_trt2")
result_trt1_trt2 <- cvm_permutation_test(trt1_group, trt2_group,
                                         plotting = T,
                                         save_plot = T,
                                         name_plot = "plant_trt1_trt2")

# Stampa del risultato
cat("W^2 osservato:", result_ctrl_trt1$W_squared_observed, "\n")
cat("p-value:", result_ctrl_trt1$p_value, "\n")
