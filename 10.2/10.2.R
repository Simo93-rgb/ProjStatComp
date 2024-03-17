#10.2 Cramér-Von Mises####

source("10.2/cvm.R")

# Caricamento dataset dei pulcini
attach(chickwts)
boxplot(formula(chickwts))
soybean <- sort(as.vector(weight[feed == "soybean"]))
casein <- sort(as.vector(weight[feed == "casein"]))
linseed <- sort(as.vector(weight[feed == "linseed"]))
sunflower <- sort(as.vector(weight[feed == "sunflower"]))
detach(chickwts)
length(linseed)
length(sunflower)
# Caricamento del dataset PlantGrowth
attach(PlantGrowth)
boxplot(formula(PlantGrowth))

# Suddivisione del dataset in base ai gruppi di trattamento
ctrl_group <- PlantGrowth$weight[PlantGrowth$group == 'ctrl']
trt1_group <- PlantGrowth$weight[PlantGrowth$group == 'trt1']
trt2_group <- PlantGrowth$weight[PlantGrowth$group == 'trt2']

detach(PlantGrowth)
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
