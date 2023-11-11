# Set seed for reproducibility
set.seed(123)

# Generate 100 points from the exponential distribution E(4)
data.exp <- rexp(100, rate = 4)

# Plot histogram of the sample data
hist(data.exp, main = "Histogram of Exponential Data", xlab = "Value", col = "lightblue", freq =  FALSE)

# Overlay the theoretical density ofnthe histogram
curve(dexp(x, rate = 4), add = TRUE, col = "darkred", lwd = 2)

# Add a legend
legend("topright", legend = c("Histogram", "Exponential Density"), col = c("lightblue", "darkred"), lwd = 2)

# ================================================================================================================================
  QUESTION 7
# ===============================================================================================================================
  
# Calculate  probabilities for P(X = k) for k in {0, 1, ..., 10} and lambda = 2
lmbda <- 2
k_value <- 0:10
probabilities <- dpois(k_value, lmbda)

# Create a bar graph for the sample data data.pois
data.pois <- rpois(300, lmbda) # Generating 300 random samples from Poisson(2)
bar_graph <- table(data.pois) / 300

# Plot the bar graph
barplot(bar_graph, main = "Bar Graph of Sample Data", xlab = "Value", ylab = "Probability", col = "lightblue")

# Overlay the calculated probabilities on the bar graph
points(k_value + 1, probabilities, col = "darkred", pch = 16)

# Add a legent
legend("topright", legend = c("Sample Data", "P(X = k)"), col = c("lightblue", "darkred"), pch = c(15, 16))


# INTERPRETATION
# The bar graph represents the distribution of the sample data.
# The overlaid red points represent the calculated probabilities for P(X = k) for each value of k.
# Compare the sample data distribution with the theoretical Poisson ditribution to assess their similarity.

========================================================================
========================================================================
  berTLC = function(n) {
    X = seq(0, n)
    p = dbinom(X, n, 0.5)
    return(p)
  }

par(mfrow = c(2, 5))
plot(seq(0, 1), c(0.5, 0.5), ylim = c(0, 0.6), xlim = c(0, 2), type = "l", xlab = "X",
     ylab = "Freq", main = "n=1", col = "red")

for (n in seq(2, 10)) {
  plot(seq(0, n), berTLC(n), ylim = c(0, 0.6), xlim = c(0, n + 1), type = "l", xlab = "X",
       ylab = "Freq", col = "red", main = bquote(paste(,"n=", .(n))))
}

==========================================================================
==========================================================================
  dieTLC = function(n) {
    X = seq(1, 6 * n)
    p = dgeom(X, 1/6)
    return(p)
  }



par(mfrow = c(2, 2))
for (n in seq(1, 4)) {
  plot(seq(1, 6 * n), dieTLC(n), ylim = c(0, 0.3), type = "l", xlab = "X",
       ylab = "Freq", col = "blue", main = bquote(paste(,"n=", .(n))))
}

