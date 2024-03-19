source("10.2/cvm.R")
source("10.3/cvm_permutation_test.R")
attach(chickwts)
boxplot(formula(chickwts))
soybean_sorted <- sort(as.vector(weight[feed == "soybean"]))
soybean <- (weight[feed == "soybean"])
casein <- (weight[feed == "casein"])
linseed <- (weight[feed == "linseed"])
linseed_sorted <- sort(as.vector(weight[feed == "linseed"]))
sunflower_sorted <- sort(as.vector(weight[feed == "sunflower"]))
detach(chickwts)

r <- rnorm(50)
r_sorted <- sort(as.vector(r))
distanza <- cramer_von_mises(soybean_sorted, soybean_sorted)
distanza <- cramer_von_mises(linseed, linseed)
distanza <- cramer_von_mises(linseed_sorted, linseed_sorted)
distanza <- cramer_von_mises(r_sorted, r_sorted)
distanza <- cvm_permutation_test(runif(100), sample(seq(0,1, by=0.000000001),100,replace = T))
cbind(rank(r),rank(r_sorted))
cbind(runif(10), sample(0:1,10,replace = T))
cbind(runif(100), sample(seq(0,1, by=0.000000001),100,replace = T))
sample_x<-linseed_sorted
u <- numeric(12)
  combinati <- c(sample_x, sample_x)
  ranked_sample <- rank(combinati)
  rank_x <- ranked_sample[1:12]

  rank_y <- ranked_sample[(12+1):(24)]
for (i in 1:12){
  print(rank_x[i])
  u[i] <- ( 12*(rank_x[i]-i)^2 + 12*(rank_y[i]-i)^2 )
  print(u)
}
print(sample(0:1,10,replace = T))
for (i in 10){
   print(sample(0:1,10,replace = T))
}
sum(u)

cbind(mean(sample(0:1,1000,replace = T)), mean(runif(1000)))