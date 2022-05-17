library(mvtnorm)
library(xgboost)
library(treeshap)
library(data.table)

source("rcpp_implementation.R")

set.seed(44)

# Simulated
n <- 1000
p <- 4
beta <- c(2, 1, .5, 0)
beta0 <- 0
cov_base <- 0
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
x[, 3] <- rbinom(n, 1, .5)
x[, 4] <- rbinom(n, 1, .5)
lp <- x %*% beta + beta0 + 10 * x[, 1] * x[, 3] * x[, 4]
y <- lp + rnorm(n)
dat <- data.frame(y = y, x)

# Ground truth Shapley values
shapley <- sweep(x, 2, beta, "*")
#inte

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 10, eta = .5), nrounds = 10)

res <- myshap_rcpp(xg, x)

plot(x[, 1], res$m[, "x1"])
#plot(x[, 4], res$m[, "x4"])

#plot(x[, 1], res$m[, "x1:x4"], col = x[, 4])

library(ggplot2)
qplot(x[, 1], res$m[, "x1:x3:x4"], col = x[, 4], shape = factor(x[, 3])) +
  scale_shape_manual(values=c(3, 17)) +
  scale_colour_gradient(low="red", high="blue")

#plot(x[, 1], res$shap[, "x1"])
#plot(x[, 4], res$shap[, "x4"])

#boxplot(res$m)
#boxplot(res$interactions)
