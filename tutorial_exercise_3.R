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
