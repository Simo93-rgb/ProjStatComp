#10.2 Cramér-Von Mises

cramer_von_mises <- function(sample_x, sample_y) {
  n <- length(sample_x)
  m <- length(sample_y)
  ranked_sample <- rank(c(sample_x, sample_y))
  rank_x <- ranked_sample[1:n]
  rank_y <- ranked_sample[n+1:m]
  
  U <- n * sum( (rank_x-(1:n))^2 ) + m * sum( (rank_y-(1:m))^2 )
  
  W_squared <- U / ( n*m*(n+m) ) - ( (4*n*m) -1 ) / ( 6*(n+m) ) 
  
  return (W_squared)
}

attach(chickwts)
boxplot(formula(chickwts))
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)


res <- cramer_von_mises(x,y)
