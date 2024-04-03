#10.2 Cramér-Von Mises####

source("10.2/cvm.R")
source("Datasets/chickwts.R")
source("Datasets/plantGrowth.R")


# distanza fra stessa dist
res_l_l <- cramer_von_mises(linseed, linseed)
# distanza fra soybean e linseed
res_s_l <- cramer_von_mises(linseed, soybean, plotting = TRUE)
res_s_l_10 <- cramer_von_mises(s,l)

# distanza fra soybean e casein
res_s_c <- cramer_von_mises(soybean, casein)

# distanza fra linseed e casein
res_l_c <- cramer_von_mises(linseed, casein, plotting = TRUE, save_plot = T, name_plot = "cvm_linseed_casein")

# distanza plantgrowth
res_trt1_trt2 <- cramer_von_mises(trt1_group, trt2_group, plotting = TRUE, save_plot = T, name_plot = "cvm_trt1_trt2")
