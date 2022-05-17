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
lp <- x %*% beta + beta0
y <- lp + rnorm(n)
dat <- data.frame(y = y, x)

# Ground truth Shapley values
shapley <- sweep(x, 2, beta, "*")

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 2, eta = .5), nrounds = 10)

res <- myshap_rcpp(xg, x)

shapley[1:10, ]
res$shap[1:10, ]

plot(shapley[, 1], res$shap[, 1])

plot(x[, 1], res$m[, "x1"])

res$m
colMeans(res$m)

pred <- predict(xg, x)
(rowSums(res$shap) + mean(pred))[1:10]
pred[1:10]
plot((rowSums(res$shap) + mean(pred)), pred)

(rowSums(res$m[, -1]) + + mean(pred))[1:10]
pred[1:10]
plot((rowSums(res$m[, -1]) + mean(pred)), pred)
