#--------------------------------------------------------------------
# Likelihood for Unif(0, theta)
#--------------------------------------------------------------------

n <- 20
reps <- 100
theta <- 7

X <- matrix(runif(n * reps, 0, theta), nrow = reps)
X
X[1, ]
X[1:3, 6:10]

X_max <- apply(X, 1, max)
length(X_max)

hist(X_max, breaks = 15)


# Make a function


simulate_unif <- function(n, reps, theta = 7) {
  X <- matrix(runif(n * reps, 0, theta), nrow = reps)
  X_max <- apply(X, 1, max)
  hist(X_max, breaks = 15)
}

simulate_unif(50, 200)

#--------------------------------------------------------------------
# Standardization
#--------------------------------------------------------------------

simulate_unif_stnd <- function(n, reps, theta = 7) {
  X <- matrix(runif(n * reps, 0, theta), nrow = reps)
  X_max <- apply(X, 1, max)
  Y <- -n * (X_max - theta) / theta
  par(mfrow = c(2, 1))
  hist(X_max, breaks = 15)
  plot(density(Y), lwd = 2, main = "Density of Y_n")
  curve(dexp(x), from = 0, to = 7, add = T, col = "red", lwd = 2)
  legend("topright", c("Density of Y", "Exp(1)"), col = c("black", "red"), lty = c(1, 1), lwd = c(2, 2))
}

simulate_unif_stnd(10, 20)
simulate_unif_stnd(20, 50)
simulate_unif_stnd(50, 500)
simulate_unif_stnd(500, 2000)
par(mfrow = c(1, 1))


#--------------------------------------------------------------------
# Bayes
#--------------------------------------------------------------------

# Beta-Binomial

# Let X_1, ..., X_n ~ Bin(n, p), p ~ Beta(1, 1)

alpha <- 2
beta <- 3

curve(dbeta(x, alpha, beta), from = 0, to = 1)

# ----
n <- 20
theta <- 0.6
X <- rbinom(n, 1, theta)

lik_bern <- function(thetas, X) {
  wins <- sum(X)
  fails <- length(X) - sum(X)
  results <- c()
  for (k in 1:length(thetas))
    results[k] <- prod(thetas[k]^wins * (1 - thetas[k])^fails)
  return(results)
}

plot_bayes <- function(X, alpha, beta) {
  par(mfrow = c(3, 1))
  curve(dbeta(x, alpha, beta), from = 0, to = 1, main = "Prior")
  curve(lik_bern(x, X), from = 0, to = 1, main = "Likelihood")
  curve(dbeta(x, alpha + sum(X), beta + length(X) - sum(X) ), from = 0, to = 1, main = "Posterior")
}

plot_bayes(X, alpha = 2, beta = 17)

