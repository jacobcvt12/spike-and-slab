# generate data for linear model
set.seed(1)
n <- 1000
k <- 7
X <- cbind(1, matrix(rnorm(n * 6), nrow=n, ncol=k-1))
b <- rbind(14, 4, 5.7, 1, -3.2, 0, 0)
sigma.2 <- 1.5
epsilon <- rnorm(n, sd=sqrt(sigma.2))
y <- X %*% b + epsilon
data <- cbind(y, X)
saveRDS(data, "data/lm.RDS")
