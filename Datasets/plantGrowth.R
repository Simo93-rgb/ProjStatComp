# Caricamento del dataset PlantGrowth
attach(PlantGrowth)
boxplot(formula(PlantGrowth))

ctrl_group <- sort(weight[group == 'ctrl'])
trt1_group <- sort(weight[group == 'trt1'])
trt2_group <- sort(weight[group == 'trt2'])

detach(PlantGrowth)