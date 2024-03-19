#10.2 Cramér-Von Mises####

source("10.2/cvm.R")
source("Datasets/chickwts.R")
source("Datasets/plantGrowth.R")


# distanza fra stessa dist
res_s_l <- cramer_von_mises(linseed, linseed)

# distanza fra soybean e linseed
res_s_l <- cramer_von_mises(soybean, linseed, plotting = TRUE)

# distanza fra soybean e casein
res_s_c <- cramer_von_mises(soybean, casein)

# distanza fra linseed e casein
res_l_c <- cramer_von_mises(linseed, casein, plotting = TRUE, save_plot = T, name_plot = "prova")
