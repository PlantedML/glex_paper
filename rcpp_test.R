library(mvtnorm)
library(xgboost)
library(treeshap)
library(data.table)

source("rcpp_implementation.R")

set.seed(44)

# Simulated
n <- 100
p <- 4
beta <- rep(c(2, 1, .5, 0), p/4)
beta0 <- 0
cov_base <- 0
sigma <- toeplitz(cov_base^(0:(p-1)))
x <- matrix(rmvnorm(n = n, sigma = sigma), ncol = p,
            dimnames = list(NULL, paste0('x', seq_len(p))))
lp <- x %*% beta + beta0
y <- lp + rnorm(n)
dat <- data.frame(y = y, x)

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 2, eta = .5), nrounds = 1)


interactions <- myshap_rcpp(xg, x[1:10, ])

# SHAP values are the sum of the m's *1/d
shap <- sapply(colnames(x), function(col) {
  idx <- grep(col, colnames(interactions))
  rowSums(interactions[, idx])
})

shap
interactions

# treeshap for comparison
uxg <- xgboost.unify(xg, x[1:10, , drop = FALSE])
treeshap(uxg, x[1:10, , drop = FALSE])

# Interaction shap
treeshap(uxg, x[1:10, , drop = FALSE], interactions = TRUE)
