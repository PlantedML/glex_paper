
library(mvtnorm)
library(xgboost)
library(treeshap)
library(data.table)

source("faster_implementation_function.R")

set.seed(44)

# Simulated
n <- 100
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

# xgboost
xg <- xgboost(data = x, label = y, params = list(max_depth = 3, eta = .5), nrounds = 1)


interactions <- myshap_fast(xg, x[1:2, ])

# SHAP values are the sum of the m's *1/d
shap <- sapply(colnames(x), function(col) {
  idx <- grep(col, rownames(interactions))
  colSums(interactions[idx, ])
})

shap
interactions

# treeshap for comparison
uxg <- xgboost.unify(xg, x[1, , drop = FALSE])
treeshap(uxg, x[1:2, , drop = FALSE])

# Interaction shap
treeshap(uxg, x[1:2, , drop = FALSE], interactions = TRUE)

# TODO: Question: Our advantage? Higher-order interactions, faster? Or same speed?
# TODO: Fast implementation: Buffer results of recurse(...)? Vectorize observations? Just compute once per leaf and re-use!

# Idea: Pre-compute all possible subsets U. Go only once through the tree and compute for all subsets. Do that once for all leaves or on demand when they are reached?
