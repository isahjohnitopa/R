tclbernoulli <- function(N, p) {
  # Simulate 1000
  simulation <- matrix(rbind(1000 * N, 1, p), ncol = N, byrow = TRUE)
  
  # Calculate S_N FOR EACH VECTOR
  S_N_value <- apply(simulation, 2, function(x) {
    X_bar_N <- mean(x)
    S_N <- (X_bar_N - p) /sqrt((p * (1 - p)) / N)
    return(S_N)
  })
  
  return(S_N_value)
}


# Example usage:
set.seed(123) # Set seed for reproducibility
N <- 10
p <- 0.3
result <- tclbernoulli(N, p)


# Display the result
head(result)



# ------- 1 ----------
tclbernoulli <- function(N,p){
  nb.echant <- 1000 
  res <- rep(0,nb.echant) 
  for (i in 1:nb.echant){
    data <- rbinom(N,1,p) 
    moy.emp <- mean(data) 
    res[i] <- sqrt(N/(p*(1-p)))*(moy.emp-p) 
  }
  return(res)
}

# ------- 2 ----------

s<-tclbernoulli(10,0.2)
mean(abs(s)<1.96)      
# Do the same for N=100 and N=1000
#...

# ------- 3 ----------
par(mfrow=c(1,3))

res <- tclbernoulli(1000,.2) 
fdr10 <- ecdf(res)
plot.ecdf(res,main="FdR, N=1000",cex=0.5)
lines(sort(res),pnorm(sort(res)),col='red')