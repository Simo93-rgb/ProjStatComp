source("Datasets/chickwts.R")

print(cbind(linseed=linseed, soybean=soybean))
ranghi <- rank(c(sort(linseed), sort(soybean)))
rango_linseed<- ranghi[seq_along(linseed)]
rango_soybean<-ranghi[(length(linseed)+1):(length(linseed)+length(soybean))]
print(cbind(rango_linseed= rango_linseed, rango_soybean=rango_soybean))
