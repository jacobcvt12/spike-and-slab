library(R2jags)

# define model
model <- function() {
    for(i in 1:n) {
        y[i]  ~ dnorm(mu[i],inv.var)
        mu[i] <- beta[1] * X[i, 1] +
                 beta[2] * X[i, 2] +
                 beta[3] * X[i, 3] +
                 beta[4] * X[i, 4] +
                 beta[5] * X[i, 5] +
                 beta[6] * X[i, 6] +
                 beta[7] * X[i, 7]
    }
    
    # Prior for beta
    for(j in 1:7) {
        beta[j] ~ dnorm(0,0.0001)
    }
    
    # Prior for the inverse variance
    inv.var ~ dgamma(0.01, 0.01)
    sigma <- 1/sqrt(inv.var)
}

# read in data
data <- readRDS("data/lm.RDS")
y <- as.vector(data[, 1])
X <- data[, 2:8]
n <- nrow(X)

model.data <- c("y", "X", "n")
model.params <- c("beta", "sigma")

fit <- jags(model.data, NULL, model.params, model, n.iter=5000)
fit
