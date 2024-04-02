# Caricamento dataset dei pulcini
attach(chickwts)
boxplot(formula(chickwts))

soybean <- (as.vector(weight[feed == "soybean"]))
casein <- (as.vector(weight[feed == "casein"]))
linseed <- (as.vector(weight[feed == "linseed"]))
sunflower <- (as.vector(weight[feed == "sunflower"]))

detach(chickwts)