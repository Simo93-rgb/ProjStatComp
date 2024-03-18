# Caricamento dataset dei pulcini
attach(chickwts)
boxplot(formula(chickwts))

soybean <- sort(as.vector(weight[feed == "soybean"]))
casein <- sort(as.vector(weight[feed == "casein"]))
linseed <- sort(as.vector(weight[feed == "linseed"]))
sunflower <- sort(as.vector(weight[feed == "sunflower"]))

detach(chickwts)